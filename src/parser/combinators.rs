use std::cell::RefCell;

use super::error::Error;
use super::locations::Span;
use super::memo::ParserCache;
use super::tokenizer::{Token, TokenType};

use derivative::Derivative;

#[derive(Debug, Clone)]
pub enum ParseResult<'a, Output> {
    Ok((Output, ParserState<'a>)),
    Err,
}

#[derive(Debug, Clone, Copy)]
pub enum Pass {
    FirstScan,
    ErrorLocation,
}

#[derive(Derivative)]
#[derivative(Debug, Clone, Copy)]
pub struct ParserState<'a> {
    pub(super) tokens: &'a [Token],
    pub(super) errors: &'a RefCell<Vec<Error>>,
    #[derivative(Debug="ignore")]
    pub(super) cache: &'a RefCell<ParserCache<'a>>,
    pub(super) pass: Pass,
}

impl<'a> ParserState<'a> {
    pub(super) fn new(
        input: &'a [Token],
        errors: &'a RefCell<Vec<Error>>,
        cache: &'a RefCell<ParserCache<'a>>,
        pass: Pass,
    ) -> Self {
        Self {
            tokens: input,
            errors,
            cache,
            pass,
        }
    }
    pub fn report_error(&self, error: Error) {
        self.errors.borrow_mut().push(error);
    }
    pub fn next_span(&self) -> Span {
        match self.tokens.first() {
            Some(token) => token.span,
            None => unreachable!(),
        }
    }
    #[inline]
    pub fn consume(&self) -> Self {
        Self {
            tokens: &self.tokens[1..],
            errors: self.errors,
            cache: self.cache,
            pass: self.pass,
        }
    }
}

impl<'a, T> ParseResult<'a, T> {
    #[inline]
    pub(super) fn into<S>(self) -> ParseResult<'a, S>
    where
        S: From<T>,
    {
        match self {
            Self::Ok((output, rest)) => ParseResult::Ok((output.into(), rest)),
            Self::Err => ParseResult::Err,
        }
    }
    #[inline]
    pub(super) fn or_else<O>(self, op: O) -> Self
    where
        O: FnOnce() -> Self,
    {
        match self {
            Self::Ok(_) => self,
            Self::Err => op(),
        }
    }
    #[inline]
    pub(super) fn map<U, F>(self, op: F) -> ParseResult<'a, U>
    where
        F: FnOnce((T, ParserState<'a>)) -> (U, ParserState<'a>),
    {
        match self {
            Self::Ok(inner) => ParseResult::Ok(op(inner)),
            Self::Err => ParseResult::Err,
        }
    }
    #[inline]
    pub(super) fn and_then<U, F>(self, op: F) -> ParseResult<'a, U>
    where
        F: FnOnce((T, ParserState<'a>)) -> ParseResult<'a, U>,
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
    fn parse(&self, input: ParserState<'a>) -> ParseResult<'a, Output>;
    #[inline]
    fn map<F, MappedOutput>(self, map_fn: F) -> impl Parser<'a, MappedOutput>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(Output) -> MappedOutput,
    {
        map(self, map_fn)
    }
    #[inline]
    fn pred<F>(self, predicate: F) -> impl Parser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn(&Output) -> bool,
    {
        pred(self, predicate)
    }
    #[inline]
    fn discard(self) -> impl Parser<'a, ()>
    where
        Self: Sized + 'a,
        Output: 'a,
    {
        map(self, |_| ())
    }
    #[inline]
    fn or(self, parser: impl Parser<'a, Output> + 'a) -> impl Parser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
    {
        move |input| self.parse(input).or_else(|| parser.parse(input))
    }
    fn expect<F>(self, error_fn: F) -> impl Parser<'a, Output>
    where
        Self: Sized + 'a,
        Output: 'a,
        F: Fn() + 'a,
    {
        map_err(self, error_fn)
    }
}

impl<'a, F, Output> Parser<'a, Output> for F
where
    F: Fn(ParserState<'a>) -> ParseResult<Output>,
{
    #[inline]
    fn parse(&self, input: ParserState<'a>) -> ParseResult<'a, Output> {
        #[cfg(single_step_debug)] 
        {
            let n = std::any::type_name::<F>();
            if !n.contains("{{closure}}") {
                let input_str = input.tokens.iter().map(|t| format!("{t}")).collect::<Vec<_>>().join(", ");
                println!("{n} {:}", input_str);
                pause();
            }
            let res = self(input);
            if !n.contains("{{closure}}") {
                let res_txt = if matches!(res, ParseResult::Ok(_)) {
                    "Ok()"
                } else {
                    "Err"
                };
                println!("... {n} => {res_txt}");
            }
            res
        }
        #[cfg(not(single_step_debug))]
        {
            self(input)
        }
    }
}

#[cfg(single_step_debug)]
fn pause() {
    use std::io;
    use std::io::prelude::*;
    let mut stdin = io::stdin();
    let mut stdout = io::stdout();
    write!(stdout, "").unwrap();
    stdout.flush().unwrap();
    let _ = stdin.read(&mut [0u8]).unwrap();
}

#[inline]
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

#[inline]
pub(super) fn map<'a, F, A, B>(
    parser: impl Parser<'a, A>,
    map_fn: F,
) -> impl Fn(ParserState<'a>) -> ParseResult<'a, B>
where
    F: Fn(A) -> B,
{
    move |input| {
        parser
            .parse(input)
            .map(|(result, rest)| (map_fn(result), rest))
    }
}

#[inline]
pub(super) fn map_err<'a, F, A>(
    parser: impl Parser<'a, A>,
    error_fn: F,
) -> impl Fn(ParserState<'a>) -> ParseResult<'a, A>
where
    F: Fn(),
{
    move |input| match parser.parse(input) {
        ParseResult::Err => {
            error_fn();
            ParseResult::Err
        }
        ParseResult::Ok(inner) => ParseResult::Ok(inner),
    }
}

#[inline]
pub(super) fn left<'a, A, B>(
    left_parser: impl Parser<'a, A>,
    right_parser: impl Parser<'a, B>,
) -> impl Parser<'a, A> {
    map(pair(left_parser, right_parser), |(left, _right)| left)
}

#[inline]
pub(super) fn right<'a, A, B>(
    left_parser: impl Parser<'a, A>,
    right_parser: impl Parser<'a, B>,
) -> impl Parser<'a, B> {
    map(pair(left_parser, right_parser), |(_left, right)| right)
}

#[inline]
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

#[inline]
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

#[inline]
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

#[inline]
pub(super) fn maybe<'a, R>(parser: impl Parser<'a, R>) -> impl Parser<'a, Option<R>> {
    move |input| match parser.parse(input) {
        ParseResult::Ok((value, rest)) => ParseResult::Ok((Some(value), rest)),
        ParseResult::Err => ParseResult::Ok((None, input)),
    }
}

#[inline]
pub(super) fn tok(expected_type: TokenType) -> impl Fn(ParserState) -> ParseResult<Token> {
    move |input| match input.tokens.first() {
        Some(token) if token.typ == expected_type => {
            ParseResult::Ok((token.clone(), input.consume()))
        }
        _ => ParseResult::Err,
    }
}

#[inline]
pub(super) fn token(
    expected_type: TokenType,
    expected_lexeme: &'static str,
) -> impl Fn(ParserState) -> ParseResult<()> {
    move |input| match input.tokens.first() {
        Some(token) if token.typ == expected_type && token.lexeme.as_ref() == expected_lexeme => {
            ParseResult::Ok(((), input.consume()))
        }
        _ => ParseResult::Err,
    }
}

#[inline]
pub(super) fn token_nodiscard(
    expected_type: TokenType,
    expected_lexeme: &'static str,
) -> impl Fn(ParserState) -> ParseResult<Token> {
    move |input| match input.tokens.first() {
        Some(token) if token.typ == expected_type && token.lexeme.as_ref() == expected_lexeme => {
            ParseResult::Ok((token.clone(), input.consume()))
        }
        _ => ParseResult::Err,
    }
}

#[inline]
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

#[inline]
pub(super) fn not<'a, R>(parser: impl Parser<'a, R>) -> impl Parser<'a, ()> {
    move |input| match parser.parse(input) {
        ParseResult::Ok(_) => ParseResult::Err,
        ParseResult::Err => ParseResult::Ok(((), input)),
    }
}

#[inline]
pub(super) fn lookahead<'a, R>(parser: impl Parser<'a, R>) -> impl Parser<'a, ()> {
    move |input| match parser.parse(input) {
        ParseResult::Ok(_) => ParseResult::Ok(((), input)),
        ParseResult::Err => ParseResult::Err,
    }
}

#[inline]
pub(super) fn epsilon(input: ParserState) -> ParseResult<()> {
    ParseResult::Ok(((), input))
}

#[inline]
pub(super) fn on_error_pass<'a, R>(parser: impl Parser<'a, R>) -> impl Parser<'a, R> {
    move |input: ParserState<'a>| match input.pass {
        Pass::ErrorLocation => parser.parse(input),
        Pass::FirstScan => ParseResult::Err,
    }
}

#[inline]
pub(super) fn anything(input: ParserState) -> ParseResult<Token> {
    ParseResult::Ok((
        input.tokens.first().cloned().unwrap_or_default(),
        if input.tokens.first().is_some() {
            input.consume()
        } else {
            input
        },
    ))
}
