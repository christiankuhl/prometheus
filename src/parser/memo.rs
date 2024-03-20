use super::{Expression, ParseResult, ParserInput};
use std::rc::Rc;

pub(super) fn try_remember(
    input: ParserInput,
    caller: fn(ParserInput) -> ParseResult<Rc<Expression>>,
) -> Option<ParseResult<Rc<Expression>>> {
    let key = (input.0.as_ptr() as usize, caller as usize);
    input.1 .1.borrow().get(&key).cloned()
}

pub(super) fn save_result<'a>(
    input: ParserInput<'a>,
    caller: fn(ParserInput) -> ParseResult<Rc<Expression>>,
    result: &ParseResult<'a, Rc<Expression>>,
) {
    let key = (input.0.as_ptr() as usize, caller as usize);
    input.1 .1.borrow_mut().insert(key, result.clone());
}
