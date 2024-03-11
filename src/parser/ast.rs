use super::tokenizer::{
    Span, Token, TokenType as TT, BINNUMBER, DECNUMBER, FLOATNUMBER, HEXNUMBER, IMAGNUMBER,
    OCTNUMBER,
};

#[derive(Debug, Clone)]
pub enum Statement {
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
    With(
        Vec<Expression>,        // item 
        Vec<Statement>,         // block
        Option<Expression>,     // type comment
        bool,                   // async
    ),
    For(
        Vec<Expression>,        // targets
        Vec<Expression>,        // expression
        Vec<Statement>,         // block
        Option<Vec<Statement>>, // else block
        Option<Expression>,     // type comment
        bool,                   // async
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
    Raise(Option<Box<Expression>>, Option<Box<Expression>>),
    Match(Vec<Expression>, Vec<Expression>),
    Type(Name, Vec<Expression>, Box<Expression>),
}

#[derive(Clone)]
pub struct Name {
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
pub struct FunctionDeclaration {
    pub(super) name: Name,
    pub(super) parameters: Vec<Parameter>,
    pub(super) code: Vec<Statement>,
    pub(super) is_async: bool,
}

#[derive(Debug, Clone)]
pub struct ClassDefinition {
    pub(super) name: Name,
    pub(super) ancestors: Arguments,
    pub(super) body: Vec<Statement>,
    pub(super) decorators: Vec<Decorator>,
    pub(super) type_params: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub(super) name: Name,
    pub(super) default: Option<Expression>,
    pub(super) annotation: Option<Expression>,
    pub(super) starred: bool,
    pub(super) double_starred: bool,
    pub(super) type_comment: Option<String>,
}

impl Parameter {
    pub(super) fn with_default(name: Name, default: Expression) -> Self {
        Self {
            name,
            default: Some(default),
            annotation: None,
            starred: false,
            double_starred: false,
            type_comment: None,
        }
    }
    pub(super) fn with_annotation(name: Name, annotation: Option<Expression>) -> Self {
        Self {
            name,
            default: None,
            annotation,
            starred: false,
            double_starred: false,
            type_comment: None,
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
        }
    }
}

#[derive(Debug, Clone)]
pub struct Decorator(pub(super) Expression);

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    Capture(Option<Box<Pattern>>, Name),
    Literal(Expression),
    Value(Vec<Name>),
    Group(Box<Pattern>),
    Sequence(Vec<Pattern>),
    Star(Box<Pattern>),
    DoubleStar(Name),
    Mapping(Vec<Pattern>),
    Class(Box<Expression>, Vec<Pattern>),
    Disjunction(Vec<Pattern>),
    KeyValue(Box<Expression>, Box<Pattern>),
}

#[derive(Debug, Clone)]
pub enum Expression {
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
    ForIfClause(Vec<Expression>, Box<Expression>, Vec<Expression>, bool),
    Tuple(Vec<Expression>),
    List(Vec<Expression>),
    ListComprehension(Box<Expression>, Vec<Expression>),
    DictUnwrap(Box<Expression>),
    Dict(Vec<Expression>),
    DictComprehension(Box<Expression>, Vec<Expression>),
    Set(Vec<Expression>),
    SetComprehension(Box<Expression>, Vec<Expression>),
    KeywordArgument(Name, Box<Expression>),
    FStringReplacement(FStringReplacement),
    Name(Name),
    Number(Number),
    Ellipsis,
    True,
    False,
    None,
    Case(Vec<Pattern>, Option<Box<Expression>>, Vec<Statement>),
    TypeBound(TypeBound),
    Pattern(Box<Pattern>),
    Attribute(Vec<Name>),
    Lambda(Vec<Parameter>, Box<Expression>),
    TypeComment(String),
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
pub struct TypeBound {
    pub(super) name: Name,
    pub(super) type_bound: Option<Box<Expression>>,
    pub(super) starred: bool,
    pub(super) double_starred: bool,
}

#[derive(Debug, Clone)]
pub enum PyString {
    Literal(String),
    FString(Vec<FString>),
}

#[derive(Debug, Clone)]
pub enum FString {
    Literal(String),
    Interpolated(FStringReplacement),
}

#[derive(Debug, Clone)]
pub struct FStringReplacement {
    pub(super) exprs: Vec<Expression>,
    pub(super) debug: bool,
    pub(super) conversion: Option<Name>,
    pub(super) format_specs: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub enum Number {
    Int(i64),
    Float(f64),
    Complex(f64, f64),
}

impl From<Token> for Number {
    fn from(value: Token) -> Self {
        let mut value = value.lexeme;
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
pub struct Argument;

#[derive(Debug, Clone)]
pub struct Arguments {
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
pub enum Slice {
    Simple(Expression),
    Delimited(Option<Expression>, Option<Expression>, Option<Expression>),
}

impl From<Expression> for Slice {
    fn from(value: Expression) -> Self {
        Self::Simple(value)
    }
}

#[derive(Debug, Clone)]
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
    pub(super) name: Vec<Name>, // convention: empty Vec serves as *
    pub(super) alias: Option<Name>,
}
