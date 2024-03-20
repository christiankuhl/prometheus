use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::{Expression, ParseResult, ParserState, Statement};

pub(super) fn try_remember<R>(
    input: ParserState,
    caller: fn(ParserState) -> ParseResult<Rc<R>>,
) -> Option<ParseResult<Rc<R>>>
where
    R: Cacheable,
{
    let key = (input.tokens.as_ptr() as usize, caller as usize);
    R::try_load(&input.cache, key)
}

pub(super) fn save_result<'a, R>(
    input: ParserState<'a>,
    caller: fn(ParserState) -> ParseResult<Rc<R>>,
    result: &ParseResult<'a, Rc<R>>,
) where
    R: Cacheable,
{
    let key = (input.tokens.as_ptr() as usize, caller as usize);
    R::store(input.cache, key, result);
}

trait Cacheable {
    fn try_load<'a>(
        cache: &'a RefCell<ParserCache<'a>>,
        key: (usize, usize),
    ) -> Option<ParseResult<'a, Rc<Self>>>;
    fn store<'a>(
        cache: &'a RefCell<ParserCache<'a>>,
        key: (usize, usize),
        value: &ParseResult<'a, Rc<Self>>,
    );
}

#[derive(Debug, Clone)]
pub(super) struct ParserCache<'a> {
    expressions: HashMap<(usize, usize), ParseResult<'a, Rc<Expression>>>,
    statements: HashMap<(usize, usize), ParseResult<'a, Rc<Statement>>>,
    blocks: HashMap<(usize, usize), ParseResult<'a, Rc<Vec<Statement>>>>,
}

impl<'a> ParserCache<'a> {
    pub fn new() -> Self {
        Self {
            expressions: HashMap::new(),
            statements: HashMap::new(),
            blocks: HashMap::new(),
        }
    }
}

impl Cacheable for Expression {
    fn try_load<'a>(
        cache: &'a RefCell<ParserCache<'a>>,
        key: (usize, usize),
    ) -> Option<ParseResult<'a, Rc<Self>>> {
        cache.borrow().expressions.get(&key).cloned()
    }
    fn store<'a>(
        cache: &'a RefCell<ParserCache<'a>>,
        key: (usize, usize),
        value: &ParseResult<'a, Rc<Self>>,
    ) {
        cache.borrow_mut().expressions.insert(key, value.clone());
    }
}

impl Cacheable for Statement {
    fn try_load<'a>(
        cache: &'a RefCell<ParserCache<'a>>,
        key: (usize, usize),
    ) -> Option<ParseResult<'a, Rc<Self>>> {
        cache.borrow().statements.get(&key).cloned()
    }
    fn store<'a>(
        cache: &'a RefCell<ParserCache<'a>>,
        key: (usize, usize),
        value: &ParseResult<'a, Rc<Self>>,
    ) {
        cache.borrow_mut().statements.insert(key, value.clone());
    }
}
