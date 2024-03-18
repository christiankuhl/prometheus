use super::{ParseResult, ParserInput, Expression};

pub(super) fn try_remember(input: ParserInput, caller: fn(ParserInput) -> ParseResult<Expression>) -> Option<ParseResult<Expression>> {
    let key = (input.0.as_ptr() as usize , caller as usize);
    input.1.1.borrow().get(&key).cloned()
}

pub(super) fn save_result<'a>(input: ParserInput<'a>, caller: fn(ParserInput) -> ParseResult<Expression>, result: &ParseResult<'a, Expression>) {
    let key = (input.0.as_ptr() as usize , caller as usize);
    input.1.1.borrow_mut().insert(key, result.clone());
}