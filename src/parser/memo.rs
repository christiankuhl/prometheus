use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::{Expression, ParseResult, ParserState, Statement, Pattern};

pub(super) fn try_remember<R>(
    input: ParserState,
    caller: fn(ParserState) -> ParseResult<R>,
) -> Option<ParseResult<R>>
where
    R: Cacheable,
{
    let key = (input.tokens.as_ptr() as usize, caller as usize);
    R::try_load(input.cache, key)
}

pub(super) fn save_result<'a, R>(
    input: ParserState<'a>,
    caller: fn(ParserState) -> ParseResult<R>,
    result: &ParseResult<'a, R>,
) where
    R: Cacheable,
{
    let key = (input.tokens.as_ptr() as usize, caller as usize);
    R::store(input.cache, key, result);
}

pub(super) trait Cacheable: Sized {
    fn try_load<'a>(
        cache: &'a RefCell<ParserCache<'a>>,
        key: (usize, usize),
    ) -> Option<ParseResult<'a, Self>>;
    fn store<'a>(
        cache: &'a RefCell<ParserCache<'a>>,
        key: (usize, usize),
        value: &ParseResult<'a, Self>,
    );
}

#[derive(Debug, Clone)]
pub(super) struct ParserCache<'a> {
    expressions: HashMap<(usize, usize), ParseResult<'a, Rc<Expression>>>,
    statements: HashMap<(usize, usize), ParseResult<'a, Rc<Statement>>>,
    blocks: HashMap<(usize, usize), ParseResult<'a, Vec<Rc<Statement>>>>,
    patterns: HashMap<(usize, usize), ParseResult<'a, Rc<Pattern>>>,
}

impl<'a> ParserCache<'a> {
    pub fn new() -> Self {
        Self {
            expressions: HashMap::new(),
            statements: HashMap::new(),
            blocks: HashMap::new(),
            patterns: HashMap::new(),
        }
    }
}

impl Cacheable for Rc<Expression> {
    fn try_load<'a>(
        cache: &'a RefCell<ParserCache<'a>>,
        key: (usize, usize),
    ) -> Option<ParseResult<'a, Self>> {
        cache.borrow().expressions.get(&key).cloned()
    }
    fn store<'a>(
        cache: &'a RefCell<ParserCache<'a>>,
        key: (usize, usize),
        value: &ParseResult<'a, Self>,
    ) {
        cache.borrow_mut().expressions.insert(key, value.clone());
    }
}

impl Cacheable for Rc<Statement> {
    fn try_load<'a>(
        cache: &'a RefCell<ParserCache<'a>>,
        key: (usize, usize),
    ) -> Option<ParseResult<'a, Self>> {
        cache.borrow().statements.get(&key).cloned()
    }
    fn store<'a>(
        cache: &'a RefCell<ParserCache<'a>>,
        key: (usize, usize),
        value: &ParseResult<'a, Self>,
    ) {
        cache.borrow_mut().statements.insert(key, value.clone());
    }
}

impl Cacheable for Vec<Rc<Statement>> {
    fn try_load<'a>(
        cache: &'a RefCell<ParserCache<'a>>,
        key: (usize, usize),
    ) -> Option<ParseResult<'a, Self>> {
        cache.borrow().blocks.get(&key).cloned()
    }
    fn store<'a>(
        cache: &'a RefCell<ParserCache<'a>>,
        key: (usize, usize),
        value: &ParseResult<'a, Self>,
    ) {
        cache.borrow_mut().blocks.insert(key, value.clone());
    }
}

impl Cacheable for Rc<Pattern> {
    fn try_load<'a>(
        cache: &'a RefCell<ParserCache<'a>>,
        key: (usize, usize),
    ) -> Option<ParseResult<'a, Self>> {
        cache.borrow().patterns.get(&key).cloned()
    }
    fn store<'a>(
        cache: &'a RefCell<ParserCache<'a>>,
        key: (usize, usize),
        value: &ParseResult<'a, Self>,
    ) {
        cache.borrow_mut().patterns.insert(key, value.clone());
    }
}