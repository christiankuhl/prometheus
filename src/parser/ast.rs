use super::locations::{Locatable, Span};
use super::tokenizer::{
    Token, TokenType as TT, BINNUMBER, DECNUMBER, FLOATNUMBER, HEXNUMBER, IMAGNUMBER, OCTNUMBER,
};

use std::rc::Rc;

pub type Block = Vec<Rc<Statement>>;

#[derive(Debug)]
pub enum Statement {
    FunctionDeclaration(FunctionDeclaration, Vec<Decorator>, Span),
    Continue(Span),
    Break(Span),
    Pass(Span),
    Expressions(Vec<Rc<Expression>>, Span),
    Return(Vec<Rc<Expression>>, Span),
    If(
        Rc<Expression>,
        Block,
        Vec<(Rc<Expression>, Block)>,
        Option<Block>,
        Span,
    ),
    ClassDefinition(ClassDefinition, Span),
    With(
        Vec<Rc<Expression>>,    // item
        Block,                  // block
        Option<Rc<Expression>>, // type comment
        bool,                   // async
        Span,
    ),
    For(
        Vec<Rc<Expression>>,    // targets
        Vec<Rc<Expression>>,    // expression
        Block,                  // block
        Option<Block>,          // else block
        Option<Rc<Expression>>, // type comment
        bool,                   // async
        Span,
    ),
    Try(
        Block,               // block
        Vec<Rc<Expression>>, // except_block
        Option<Block>,       // else_block
        Option<Block>,       // finally_block
        Span,
    ),
    While(Rc<Expression>, Block, Option<Block>, Span),
    Assignment(
        Vec<Rc<Expression>>,         // targets
        Option<Operator>,            // augassign
        Option<Vec<Rc<Expression>>>, // rhs
        Option<Rc<Expression>>,      // type
        Span,
    ),
    Del(Vec<Rc<Expression>>, Span),
    Yield(Rc<Expression>, Span),
    Assert(Rc<Expression>, Option<Rc<Expression>>, Span),
    Global(Vec<Name>, Span),
    Nonlocal(Vec<Name>, Span),
    Import(Vec<Import>, Span),
    Raise(Option<Rc<Expression>>, Option<Rc<Expression>>, Span),
    Match(Vec<Rc<Expression>>, Vec<Rc<Expression>>, Span),
    Type(Name, Vec<Rc<Expression>>, Rc<Expression>, Span),
    Invalid,
}

#[derive(Clone)]
pub struct Name {
    pub(super) name: Rc<str>,
    pub(super) span: Span,
}

impl std::fmt::Debug for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.name)
    }
}

impl From<Token> for Name {
    fn from(value: Token) -> Self {
        match value.typ {
            TT::NAME => Self {
                name: value.lexeme,
                span: value.span,
            },
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub(super) name: Name,
    pub(super) parameters: Vec<Parameter>,
    pub(super) code: Block,
    pub(super) is_async: bool,
    pub(super) return_type: Option<Rc<Expression>>,
    pub(super) func_type: Option<Rc<Expression>>,
}

#[derive(Debug, Clone)]
pub struct ClassDefinition {
    pub(super) name: Name,
    pub(super) ancestors: Arguments,
    pub(super) body: Block,
    pub(super) decorators: Vec<Decorator>,
    pub(super) type_params: Vec<Rc<Expression>>,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub(super) name: Name,
    pub(super) default: Option<Rc<Expression>>,
    pub(super) annotation: Option<Rc<Expression>>,
    pub(super) starred: bool,
    pub(super) double_starred: bool,
    pub(super) type_comment: Option<Rc<str>>,
    pub(super) keyword_only: bool,
    pub(super) positional_only: bool,
}

impl Parameter {
    pub(super) fn with_default(name: Name, default: Rc<Expression>) -> Self {
        Self {
            name,
            default: Some(default),
            annotation: None,
            starred: false,
            double_starred: false,
            type_comment: None,
            keyword_only: false,
            positional_only: false,
        }
    }
    pub(super) fn with_annotation(name: Name, annotation: Option<Rc<Expression>>) -> Self {
        Self {
            name,
            default: None,
            annotation,
            starred: false,
            double_starred: false,
            type_comment: None,
            keyword_only: false,
            positional_only: false,
        }
    }
    pub(super) fn kwargs(mut self) -> Self {
        self.double_starred = true;
        self
    }
}

impl From<Name> for Parameter {
    fn from(value: Name) -> Self {
        Self {
            name: value,
            default: None,
            annotation: None,
            starred: false,
            double_starred: false,
            type_comment: None,
            keyword_only: false,
            positional_only: false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Decorator(pub(super) Rc<Expression>);

#[derive(Debug)]
pub enum Pattern {
    Wildcard,
    Capture(Option<Rc<Pattern>>, Name),
    Literal(Rc<Expression>),
    Value(Vec<Name>),
    Group(Rc<Pattern>),
    Sequence(Vec<Rc<Pattern>>),
    Star(Rc<Pattern>),
    DoubleStar(Name),
    Mapping(Vec<Rc<Pattern>>),
    Class(Rc<Expression>, Vec<Rc<Pattern>>),
    Disjunction(Vec<Rc<Pattern>>),
    KeyValue(Rc<Expression>, Rc<Pattern>),
    Invalid(Span),
}

#[derive(Debug)]
pub enum Expression {
    ListUnwrap(Rc<Expression>, Span),
    BinaryOperation(Operator, Rc<Expression>, Rc<Expression>, Span),
    UnaryOperation(Operator, Rc<Expression>, Span),
    Subscript(Rc<Expression>, Name, Span),
    Call(Rc<Expression>, Arguments, Span),
    Slice(Rc<Expression>, Vec<Slice>, Span),
    WithItem(Rc<Expression>, Option<Rc<Expression>>, Span),
    ExceptBlock(Option<Rc<Expression>>, Option<Name>, Block, bool, Span),
    Walrus(Rc<Expression>, Rc<Expression>, Span),
    Ternary(Rc<Expression>, Rc<Expression>, Rc<Expression>, Span),
    Comparison(Rc<Expression>, Vec<(Operator, Rc<Expression>)>, Span),
    Strings(Vec<PyString>, Span),
    Yield(Vec<Rc<Expression>>, Span),
    YieldFrom(Rc<Expression>, Span),
    Generator(Rc<Expression>, Vec<Rc<Expression>>, Span),
    ForIfClause(
        Vec<Rc<Expression>>,
        Rc<Expression>,
        Vec<Rc<Expression>>,
        bool,
        Span,
    ),
    Tuple(Vec<Rc<Expression>>, Span),
    List(Vec<Rc<Expression>>, Span),
    ListComprehension(Rc<Expression>, Vec<Rc<Expression>>, Span),
    DictUnwrap(Rc<Expression>, Span),
    Dict(Vec<Rc<Expression>>, Span),
    DictComprehension(Rc<Expression>, Vec<Rc<Expression>>, Span),
    Set(Vec<Rc<Expression>>, Span),
    SetComprehension(Rc<Expression>, Vec<Rc<Expression>>, Span),
    KeywordArgument(Name, Rc<Expression>, Span),
    FStringReplacement(FStringReplacement, Span),
    Name(Name, Span),
    Number(Number, Span),
    Ellipsis(Span),
    True(Span),
    False(Span),
    None(Span),
    Case(Vec<Rc<Pattern>>, Option<Rc<Expression>>, Block, Span),
    TypeBound(TypeBound, Span),
    Pattern(Rc<Pattern>, Span),
    Attribute(Vec<Name>, Span),
    Lambda(Vec<Parameter>, Rc<Expression>, Span),
    TypeComment(Rc<str>, Span),
    FString(FString, Span),
    ImportItems(Vec<ImportItem>, Span),
    Parameters(Vec<Parameter>, Span),
    Arguments(Arguments, Span),
    Invalid(Span),
}

#[derive(Debug)]
pub(crate) enum IncompleteExpression {
    Subscript(Name, Box<IncompleteExpression>),
    Call(Arguments, Box<IncompleteExpression>),
    Slice(Vec<Slice>, Box<IncompleteExpression>),
    BinaryOperation(Operator, Rc<Expression>, Box<IncompleteExpression>),
    Generator(Rc<Expression>, Box<IncompleteExpression>),
    Invalid,
    Empty,
}

impl IncompleteExpression {
    pub(super) fn empty(&self) -> bool {
        matches!(self, Self::Empty)
    }
}

#[derive(Debug)]
pub struct TypeBound {
    pub(super) name: Name,
    pub(super) type_bound: Option<Rc<Expression>>,
    pub(super) starred: bool,
    pub(super) double_starred: bool,
}

#[derive(Debug)]
pub enum PyString {
    Literal(Rc<str>, Span),
    FString(Vec<FString>),
}

#[derive(Debug)]
pub enum FString {
    Literal(Rc<str>, Span),
    Interpolated(FStringReplacement),
}

#[derive(Debug, Clone)]
pub struct FStringReplacement {
    pub(super) exprs: Vec<Rc<Expression>>,
    pub(super) debug: bool,
    pub(super) conversion: Option<Name>,
    pub(super) format_specs: Vec<Rc<Expression>>,
}

#[derive(Debug, Clone, Copy)]
pub enum Number {
    Int(i64),
    Float(f64),
    Complex(f64, f64),
}

impl From<Token> for Number {
    fn from(value: Token) -> Self {
        let mut value = value.lexeme.as_ref().to_owned();
        if DECNUMBER.is_match(&value) {
            value = value.replace('_', "");
            return Self::Int(value.parse::<i64>().unwrap());
        }
        if HEXNUMBER.is_match(&value) {
            value = value.replace('_', "");
            return Self::Int(i64::from_str_radix(&value, 16).unwrap());
        }
        if OCTNUMBER.is_match(&value) {
            value = value.replace('_', "");
            return Self::Int(i64::from_str_radix(&value, 8).unwrap());
        }
        if BINNUMBER.is_match(&value) {
            value = value.replace('_', "");
            return Self::Int(i64::from_str_radix(&value, 2).unwrap());
        }
        if FLOATNUMBER.is_match(&value) {
            return Self::Float(value.parse().unwrap());
        }
        if IMAGNUMBER.is_match(&value) {
            value = value.replace('_', "");
            value = value.replace('j', "");
            value = value.replace('J', "");
            return Self::Complex(0., value.parse().unwrap());
        }
        unreachable!()
    }
}

#[derive(Debug, Clone)]
pub struct Arguments {
    pub(super) positional: Vec<Rc<Expression>>,
    pub(super) keyword: Vec<Rc<Expression>>,
}

impl Arguments {
    pub(super) fn empty() -> Self {
        Self {
            positional: vec![],
            keyword: vec![],
        }
    }
}

#[derive(Debug)]
pub enum Slice {
    Simple(Rc<Expression>),
    Delimited(
        Option<Rc<Expression>>,
        Option<Rc<Expression>>,
        Option<Rc<Expression>>,
    ),
}

impl From<Rc<Expression>> for Slice {
    fn from(value: Rc<Expression>) -> Self {
        Self::Simple(value)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Operator {
    Or,
    And,
    Not,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    BitwiseNot,
    LeftShift,
    RightShift,
    Plus,
    Minus,
    Times,
    Divide,
    IntDivide,
    Modulo,
    Power,
    MatrixMul,
    Is,
    IsNot,
    In,
    NotIn,
    Equal,
    NotEqual,
    LessThanEqual,
    LessThan,
    GreaterThanEqual,
    GreaterThan,
}

impl From<&Token> for Operator {
    fn from(value: &Token) -> Self {
        match value.typ {
            TT::VBAR => Self::BitwiseOr,
            TT::VBAREQUAL => Self::BitwiseOr,
            TT::AMPER => Self::BitwiseAnd,
            TT::AMPEREQUAL => Self::BitwiseAnd,
            TT::CIRCUMFLEX => Self::BitwiseXor,
            TT::CIRCUMFLEXEQUAL => Self::BitwiseXor,
            TT::TILDE => Self::BitwiseNot,
            TT::LEFTSHIFT => Self::LeftShift,
            TT::LEFTSHIFTEQUAL => Self::LeftShift,
            TT::RIGHTSHIFT => Self::RightShift,
            TT::RIGHTSHIFTEQUAL => Self::RightShift,
            TT::PLUS => Self::Plus,
            TT::PLUSEQUAL => Self::Plus,
            TT::MINUS => Self::Minus,
            TT::MINEQUAL => Self::Minus,
            TT::STAR => Self::Times,
            TT::STAREQUAL => Self::Times,
            TT::SLASH => Self::Divide,
            TT::SLASHEQUAL => Self::Divide,
            TT::DOUBLESLASH => Self::IntDivide,
            TT::DOUBLESLASHEQUAL => Self::IntDivide,
            TT::PERCENT => Self::Modulo,
            TT::PERCENTEQUAL => Self::Modulo,
            TT::DOUBLESTAR => Self::Power,
            TT::DOUBLESTAREQUAL => Self::Power,
            TT::AT => Self::MatrixMul,
            TT::ATEQUAL => Self::MatrixMul,
            TT::EQEQUAL => Self::Equal,
            TT::NOTEQUAL => Self::NotEqual,
            TT::LESSEQUAL => Self::LessThanEqual,
            TT::LESS => Self::LessThan,
            TT::GREATEREQUAL => Self::GreaterThanEqual,
            TT::GREATER => Self::GreaterThan,
            _ => unreachable!(),
        }
    }
}

impl From<Token> for Operator {
    fn from(value: Token) -> Self {
        Self::from(&value)
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub(super) rel_level: usize,
    pub(super) path: Vec<Name>,
    pub(super) alias: Option<Name>,
}

#[derive(Debug, Clone)]
pub struct Import {
    pub(super) module: Module,
    pub(super) items: Vec<ImportItem>,
}

#[derive(Debug, Clone)]
pub struct ImportItem {
    pub(super) name: Name,
    pub(super) alias: Option<Name>,
}

#[derive(Debug)]
pub enum Selector {
    Name(Name),
    Slice(Vec<Slice>),
}

impl Selector {
    pub fn apply_to(self, expr: Rc<Expression>) -> Rc<Expression> {
        match self {
            Self::Name(n) => {
                let s = expr.span().till(&n);
                Rc::new(Expression::Subscript(expr, n, s))
            }
            Self::Slice(s) => {
                let sp = expr.span().till(&s);
                Rc::new(Expression::Slice(expr, s, sp))
            }
        }
    }
}
