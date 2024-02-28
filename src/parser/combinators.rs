use super::tokenizer::{Token, TokenType};

// use cache_macro::cache;
// use lru_cache::LruCache;

static mut STACK_LVL: usize = 0;
static mut CALLS: usize = 0;

#[derive(Debug)]
pub(crate) enum ParseResult<'a, Output> {
    Ok((Output, &'a [Token])),
    Err,
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
        F: FnOnce((T, &'a [Token])) -> (U, &'a [Token]),
    {
        match self {
            Self::Ok(inner) => ParseResult::Ok(op(inner)),
            Self::Err => ParseResult::Err,
        }
    }
    pub(super) fn and_then<U, F>(self, op: F) -> ParseResult<'a, U>
    where
        F: FnOnce((T, &'a [Token])) -> ParseResult<'a, U>,
    {
        match self {
            Self::Ok(inner) => op(inner),
            Self::Err => ParseResult::Err,
        }
    }
    pub(super) fn discard(self) -> ParseResult<'a, ()> {
        self.map(|(_, r)| ((), r))
    }
    pub(super) fn is_ok(&self) -> bool {
        match self {
            Self::Ok(_) => true,
            Self::Err => false,
        }
    }
}

pub(super) trait Parser<'a, Output> {
    fn parse(&self, input: &'a [Token]) -> ParseResult<'a, Output>;
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
    F: Fn(&'a [Token]) -> ParseResult<Output>,
{
    // #[cache(LruCache : LruCache::new(20))]
    fn parse(&self, input: &'a [Token]) -> ParseResult<'a, Output> {
        #[cfg(trace_parser)] {
            let n = std::any::type_name::<F>();
            unsafe { STACK_LVL += 1; CALLS += 1; }
            if !n.contains("{{closure}}") {
                let mut x = String::new();
                for i in 0..input.len().min(3) {
                    x.push_str(&format!("{}, ", input[i]));
                }
                unsafe { println!("{}{}: {} ({}) [{}]", " ".repeat(STACK_LVL), STACK_LVL, n, CALLS, x); }
                let mut foo = String::new();
                let _ = std::io::stdin().read_line(&mut foo);
            }
            let res = self(input);
            if !n.contains("{{closure}}") {
                unsafe { println!("{}{}: {} => {:?}\n", " ".repeat(STACK_LVL), STACK_LVL, n, res.is_ok()); }
            }
            unsafe { STACK_LVL -= 1; }
            res
        }
        #[cfg(not(trace_parser))]
        {
            self(input)
        }
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
    fn parse(&self, input: &'a [Token]) -> ParseResult<'a, Output> {
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
) -> impl Fn(&'a [Token]) -> ParseResult<'a, B>
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

pub(super) fn tok(
    expected_type: TokenType,
) -> impl Fn(&[Token]) -> ParseResult<Token> {
    move |input| match input.get(0) {
        Some(token) if token.typ == expected_type => ParseResult::Ok((token.clone(), &input[1..])),
        _ => ParseResult::Err,
    }
}

pub(super) fn token(
    expected_type: TokenType,
    expected_lexeme: &'static str,
) -> impl Fn(&[Token]) -> ParseResult<()> {
    move |input| match input.get(0) {
        Some(token) if token.typ == expected_type && token.lexeme.as_str() == expected_lexeme => {
            ParseResult::Ok(((), &input[1..]))
        }
        _ => ParseResult::Err,
    }
}

pub(super) fn sep_by<'a, R> (
    parser: impl Parser<'a, R>,
    sep: TokenType,
) -> impl Parser<'a, Vec<R>> {
    move |input| {
        if let ParseResult::Ok((first, rest)) = parser.parse(input) {
            let mut result = Vec::new();
            let mut tmp_input = rest;
            result.push(first);
            while let ParseResult::Ok((next, rest)) = tok(sep)
                .parse(tmp_input)
                .and_then(|(_, s)| parser.parse(s))
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
    move |input| {
        match parser.parse(input) {
            ParseResult::Ok(_) => ParseResult::Err,
            ParseResult::Err => ParseResult::Ok(((), input)),
        }
    }
}

pub(super) fn lookahead<'a, R>(parser: impl Parser<'a, R>) -> impl Parser<'a, ()> {
    move |input| {
        match parser.parse(input) {
            ParseResult::Ok(_) => ParseResult::Ok(((), input)),
            ParseResult::Err => ParseResult::Err,
        }
    }
}

pub(super) fn epsilon(input: &[Token]) -> ParseResult<()> {
    ParseResult::Ok(((), input))
}