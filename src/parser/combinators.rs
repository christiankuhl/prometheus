use std::cell::RefCell;

use super::tokenizer::{Span, Token, TokenType};

#[derive(Debug)]
pub struct Error(Span, String);

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Error: {} in {} till {}", self.1, self.0.start, self.0.end)
    }
}

#[derive(Debug)]
pub enum ParseResult<'a, Output> {
    Ok((Output, ParserInput<'a>)),
    Err,
}

#[derive(Debug, Clone, Copy)]
pub struct ParserState<'a>(&'a RefCell<Vec<Error>>);

impl<'a> ParserState<'a> {
    pub fn new(errors: &'a RefCell<Vec<Error>>) -> Self {
        Self(errors)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ParserInput<'a>(&'a [Token], ParserState<'a>);

impl<'a> ParserInput<'a> {
    pub fn new(input: &'a [Token], state: ParserState<'a>) -> Self {
        Self(input, state)
    }
}

impl<'a> ParserState<'a> {
    pub(super) fn report_error(&mut self, error: Error) {
        self.0.borrow_mut().push(error);
    }
}

impl<'a, T> ParseResult<'a, T> {
    pub(super) fn into<S>(self) -> ParseResult<'a, S>
    where
        S: From<T>,
    {
        match self {
            Self::Ok((output, rest)) => ParseResult::Ok((output.into(), rest)),
            Self::Err => ParseResult::Err,
        }
    }
    pub(super) fn or_else<O>(self, op: O) -> Self
    where
        O: FnOnce() -> Self,
    {
        match self {
            Self::Ok(inner) => Self::Ok(inner),
            Self::Err => op(),
        }
    }
    pub(super) fn map<U, F>(self, op: F) -> ParseResult<'a, U>
    where
        F: FnOnce((T, ParserInput<'a>)) -> (U, ParserInput<'a>),
    {
        match self {
            Self::Ok(inner) => ParseResult::Ok(op(inner)),
            Self::Err => ParseResult::Err,
        }
    }
    pub(super) fn and_then<U, F>(self, op: F) -> ParseResult<'a, U>
    where
        F: FnOnce((T, ParserInput<'a>)) -> ParseResult<'a, U>,
    {
        match self {
            Self::Ok(inner) => op(inner),
            Self::Err => ParseResult::Err,
        }
    }
    pub(super) fn expect(self, msg: &str) -> T {
        match self {
            Self::Ok(inner) => inner.0,
            Self::Err => panic!("{}", msg),
        }
    }
}

pub(super) trait Parser<'a, Output> {
    fn parse(&self, input: ParserInput<'a>) -> ParseResult<'a, Output>;
    fn map<F, MappedOutput>(self, map_fn: F) -> BoxedParser<'a, MappedOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        MappedOutput: 'a,
        F: Fn(Output) -> MappedOutput + 'a,
    {
        BoxedParser::new(map(self, map_fn))
    }
    fn pred<F>(self, predicate: F) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(&Output) -> bool + 'a,
    {
        BoxedParser::new(pred(self, predicate))
    }
    fn discard(self) -> BoxedParser<'a, ()>
    where
        Self: Sized + 'a,
        Output: 'a,
    {
        BoxedParser::new(map(self, |_| ()))
    }
    fn or(self, parser: impl Parser<'a, Output> + 'a) -> BoxedParser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
    {
        let alternative = move |input| self.parse(input).or_else(|| parser.parse(input));
        BoxedParser::new(alternative)
    }
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(ParserInput<'a>) -> ParseResult<Output>,
{
    fn parse(&self, input: ParserInput<'a>) -> ParseResult<'a, Output> {
        self(input)
    }
}

pub(super) struct BoxedParser<'a, Output> {
    parser: Box<dyn Parser<'a, Output> + 'a>,
}

impl<'a, Output> BoxedParser<'a, Output> {
    fn new(parser: impl Parser<'a, Output> + 'a) -> Self {
        Self {
            parser: Box::new(parser),
        }
    }
}

impl<'a, Output> Parser<'a, Output> for BoxedParser<'a, Output> {
    fn parse(&self, input: ParserInput<'a>) -> ParseResult<'a, Output> {
        self.parser.parse(input)
    }
}

pub(super) fn pair<'a, R1, R2>(
    parser1: impl Parser<'a, R1>,
    parser2: impl Parser<'a, R2>,
) -> impl Parser<'a, (R1, R2)> {
    move |input| {
        parser1.parse(input).and_then(|(result1, next_input)| {
            parser2
                .parse(next_input)
                .map(|(result2, rest)| ((result1, result2), rest))
        })
    }
}

pub(super) fn map<'a, F, A, B>(
    parser: impl Parser<'a, A>,
    map_fn: F,
) -> impl Fn(ParserInput<'a>) -> ParseResult<'a, B>
where
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(result, rest)| (map_fn(result), rest))
    }
}

pub(super) fn left<'a, A, B>(
    left_parser: impl Parser<'a, A>,
    right_parser: impl Parser<'a, B>,
) -> impl Parser<'a, A> {
    map(pair(left_parser, right_parser), |(left, _right)| left)
}

pub(super) fn right<'a, A, B>(
    left_parser: impl Parser<'a, A>,
    right_parser: impl Parser<'a, B>,
) -> impl Parser<'a, B> {
    map(pair(left_parser, right_parser), |(_left, right)| right)
}

pub(super) fn pred<'a, A, F>(parser: impl Parser<'a, A>, predicate: F) -> impl Parser<'a, A>
where
    F: Fn(&A) -> bool,
{
    move |input| {
        if let ParseResult::Ok((result, rest)) = parser.parse(input) {
            if predicate(&result) {
                return ParseResult::Ok((result, rest));
            }
        }
        ParseResult::Err
    }
}

pub(super) fn one_or_more<'a, R>(parser: impl Parser<'a, R>) -> impl Parser<'a, Vec<R>> {
    move |input| {
        let mut result = Vec::new();
        let mut tmp_input;
        if let ParseResult::Ok((first, rest)) = parser.parse(input) {
            tmp_input = rest;
            result.push(first);
        } else {
            return ParseResult::Err;
        }
        while let ParseResult::Ok((next, rest)) = parser.parse(tmp_input) {
            tmp_input = rest;
            result.push(next);
        }
        return ParseResult::Ok((result, tmp_input));
    }
}

pub(super) fn zero_or_more<'a, R>(parser: impl Parser<'a, R>) -> impl Parser<'a, Vec<R>> {
    move |input| {
        let mut result = Vec::new();
        let mut tmp_input = input;
        while let ParseResult::Ok((next, rest)) = parser.parse(tmp_input) {
            tmp_input = rest;
            result.push(next);
        }
        return ParseResult::Ok((result, tmp_input));
    }
}

pub(super) fn maybe<'a, R>(parser: impl Parser<'a, R>) -> impl Parser<'a, Option<R>> {
    move |input| match parser.parse(input) {
        ParseResult::Ok((value, rest)) => ParseResult::Ok((Some(value), rest)),
        ParseResult::Err => ParseResult::Ok((None, input)),
    }
}

pub(super) fn tok(expected_type: TokenType) -> impl Fn(ParserInput) -> ParseResult<Token> {
    move |input| match input.0.first() {
        Some(token) if token.typ == expected_type => {
            ParseResult::Ok((token.clone(), ParserInput(&input.0[1..], input.1)))
        }
        _ => ParseResult::Err,
    }
}

pub(super) fn token(
    expected_type: TokenType,
    expected_lexeme: &'static str,
) -> impl Fn(ParserInput) -> ParseResult<()> {
    move |input| match input.0.first() {
        Some(token) if token.typ == expected_type && token.lexeme.as_str() == expected_lexeme => {
            ParseResult::Ok(((), ParserInput(&input.0[1..], input.1)))
        }
        _ => ParseResult::Err,
    }
}

pub(super) fn sep_by<'a, R>(parser: impl Parser<'a, R>, sep: TokenType) -> impl Parser<'a, Vec<R>> {
    move |input| {
        if let ParseResult::Ok((first, rest)) = parser.parse(input) {
            let mut result = Vec::new();
            let mut tmp_input = rest;
            result.push(first);
            while let ParseResult::Ok((next, rest)) =
                tok(sep).parse(tmp_input).and_then(|(_, s)| parser.parse(s))
            {
                tmp_input = rest;
                result.push(next)
            }
            return ParseResult::Ok((result, tmp_input));
        }
        ParseResult::Err
    }
}

pub(super) fn not<'a, R>(parser: impl Parser<'a, R>) -> impl Parser<'a, ()> {
    move |input| match parser.parse(input) {
        ParseResult::Ok(_) => ParseResult::Err,
        ParseResult::Err => ParseResult::Ok(((), input)),
    }
}

pub(super) fn lookahead<'a, R>(parser: impl Parser<'a, R>) -> impl Parser<'a, ()> {
    move |input| match parser.parse(input) {
        ParseResult::Ok(_) => ParseResult::Ok(((), input)),
        ParseResult::Err => ParseResult::Err,
    }
}

pub(super) fn epsilon(input: ParserInput) -> ParseResult<()> {
    ParseResult::Ok(((), input))
}
