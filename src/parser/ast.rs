use super::tokenizer::{Span, Token, TokenType as TT};

#[derive(Debug, Clone)]
pub(crate) enum Statement {
    FunctionDeclaration(FunctionDeclaration, Vec<Decorator>),
    Continue,
    Break,
    Pass,
    Expressions(Vec<Expression>),
    Return(Vec<Expression>),
    If(
        Expression,
        Vec<Statement>,
        Vec<(Expression, Vec<Statement>)>,
        Option<Vec<Statement>>,
    ),
    ClassDefinition(ClassDefinition),
    With(Vec<Expression>, Vec<Statement>),
    For(
        Vec<Expression>,
        Vec<Expression>,
        Vec<Statement>,
        Option<Vec<Statement>>,
    ),
    Try(
        Vec<Statement>,         // block
        Vec<Expression>,        // except_block
        Option<Vec<Statement>>, // else_block
        Option<Vec<Statement>>, // finally_block
    ),
    While(Box<Expression>, Vec<Statement>, Option<Vec<Statement>>),
    Assignment(
        Vec<Expression>,         // targets
        Option<Operator>,        // augassign
        Option<Vec<Expression>>, // rhs
        Option<Box<Expression>>, // type
    ),
    Del(Vec<Expression>),
    Yield(Box<Expression>),
    Assert(Box<Expression>, Option<Box<Expression>>),
    Global(Vec<Name>),
    Nonlocal(Vec<Name>),
    Import(Vec<Import>),
    Raise(Option<Box<Expression>>, Option<Box<Expression>>)
}

#[derive(Clone)]
pub(crate) struct Name {
    name: String,
    span: Span,
}

impl std::fmt::Debug for Name {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Name(\"{}\")", self.name)
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
pub(crate) struct FunctionDeclaration {
    pub(super) name: Name,
    pub(super) parameters: Vec<Parameter>,
    pub(super) code: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub(crate) struct ClassDefinition {
    pub(super) name: Name,
    pub(super) ancestors: Arguments,
    pub(super) body: Vec<Statement>,
    pub(super) decorators: Vec<Decorator>,
}

#[derive(Debug, Clone)]
pub(crate) struct Parameter {
    pub(super) name: Name,
    pub(super) default: Option<Expression>,
    pub(super) annotation: Option<Expression>,
    pub(super) starred: bool,
    pub(super) double_starred: bool,
}

impl Parameter {
    pub(super) fn with_default(name: Name, default: Expression) -> Self {
        Self {
            name,
            default: Some(default),
            annotation: None,
            starred: false,
            double_starred: false,
        }
    }
    pub(super) fn with_annotation(name: Name, annotation: Expression) -> Self {
        Self {
            name,
            default: None,
            annotation: Some(annotation),
            starred: false,
            double_starred: false,
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
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Decorator(pub(super) Expression);

#[derive(Debug, Clone)]
pub(crate) enum Expression {
    Starred(Box<Expression>),
    BinaryOperation(Operator, Box<(Expression, Expression)>),
    UnaryOperation(Operator, Box<Expression>),
    Subscript(Box<Expression>, Name),
    Call(Box<Expression>, Arguments),
    Slice(Box<Expression>, Vec<Slice>),
    WithItem(Box<Expression>, Option<Box<Expression>>),
    ExceptBlock(Option<Box<Expression>>, Option<Name>, Vec<Statement>, bool),
    Walrus(Box<Expression>, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
    Comparison(Box<Expression>, Vec<(Operator, Expression)>),
    Strings(Vec<PyString>),
    Yield(Vec<Expression>),
    YieldFrom(Box<Expression>),
    Generator(Box<Expression>, Vec<Expression>),
    ForIfClause(Vec<Expression>, Box<Expression>, Vec<Expression>),
    Tuple(Vec<Expression>),
    List(Vec<Expression>),
    ListComprehension(Box<Expression>, Vec<Expression>),
    DictUnwrap(Box<Expression>),
    Dict(Vec<Expression>),
    DictComprehension(Box<Expression>, Vec<Expression>),
    Set(Vec<Expression>),
    SetComprehension(Box<Expression>, Vec<Expression>),
    KeywordArgument(Name, Box<Expression>),
    Name(Name),
    Number(Number),
    Ellipsis,
    True,
    False,
    None,
    PrimaryGenexp(Box<Expression>, Box<Expression>), // ???
}

#[derive(Debug, Clone)]
pub(crate) enum IncompleteExpression {
    Subscript(Name, Box<IncompleteExpression>),
    Call(Arguments, Box<IncompleteExpression>),
    Slice(Vec<Slice>, Box<IncompleteExpression>),
    BinaryOperation(Operator, Box<Expression>, Box<IncompleteExpression>),
    PrimaryGenexp(Box<Expression>, Box<IncompleteExpression>), // ???
    Empty,
}

impl IncompleteExpression {
    pub(super) fn empty(&self) -> bool {
        matches!(self, Self::Empty)
    }
}

#[derive(Debug, Clone)]
pub(crate) enum PyString {
    Literal(String),
    FString(FString),
}

#[derive(Debug, Clone)]
pub(crate) struct FString;

#[derive(Debug, Clone)]
pub(crate) struct Number;

impl From<Token> for Number {
    fn from(value: Token) -> Self {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Argument;

#[derive(Debug, Clone)]
pub(crate) struct Arguments {
    pub(super) positional: Vec<Expression>,
    pub(super) keyword: Vec<Expression>,
}

impl Arguments {
    pub(super) fn empty() -> Self {
        Self {
            positional: vec![],
            keyword: vec![],
        }
    }
}

impl From<Expression> for Argument {
    fn from(value: Expression) -> Self {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Slice {
    Simple(Expression),
    Delimited(Option<Expression>, Option<Expression>, Option<Expression>),
}

impl From<Expression> for Slice {
    fn from(value: Expression) -> Self {
        Self::Simple(value)
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Operator {
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

impl From<Token> for Operator {
    fn from(value: Token) -> Self {
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


#[derive(Debug, Clone)]
pub(crate) struct Module {
    pub(super) rel_level: usize,
    pub(super) path: Vec<Name>,
    pub(super) alias: Option<Name>,
}

#[derive(Debug, Clone)]
pub(crate) struct Import {
    pub(super) module: Module,
    pub(super) items: Vec<ImportItem>,
}

#[derive(Debug, Clone)]
pub(crate) struct ImportItem {
    pub(super) name: Vec<Name>,     // convention: empty Vec serves as *
    pub(super) alias: Option<Name>,
}