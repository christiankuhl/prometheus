use super::ast::*;
use super::tokenizer::Token;

#[derive(Clone, Default, Debug)]
pub(crate) struct Location {
    pub(crate) line: usize,
    pub(crate) column: usize,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {}, column {}", self.line, self.column)
    }
}

pub trait Locatable {
    fn span(&self) -> Span;
}

#[derive(Clone, Default, Debug)]
pub enum Span {
    #[default]
    Indetermined,
    Determined(_Span),
}

#[derive(Clone, Default, Debug)]
pub struct _Span {
    pub(crate) start: Location,
    pub(crate) end: Location,
}

impl Span {
    pub(super) fn start(&mut self, loc: Location) {
        match self {
            Self::Determined(s) => s.start = loc,
            Self::Indetermined => {
                *self = Self::Determined(_Span {
                    start: loc,
                    end: Location::default(),
                })
            }
        }
    }
    pub(super) fn end(&mut self, loc: Location) {
        match self {
            Self::Determined(s) => s.end = loc,
            Self::Indetermined => {
                *self = Self::Determined(_Span {
                    start: Location::default(),
                    end: loc,
                })
            }
        }
    }
    pub(super) fn new(
        start_line: usize,
        start_col: usize,
        end_line: usize,
        end_col: usize,
    ) -> Self {
        Self::Determined(_Span {
            start: Location {
                line: start_line,
                column: start_col,
            },
            end: Location {
                line: end_line,
                column: end_col,
            },
        })
    }

    pub(super) fn till<R: Locatable>(&self, other: &R) -> Self {
        match (self, other.span()) {
            (Self::Indetermined, Self::Indetermined) => Self::Indetermined,
            (Self::Indetermined, Self::Determined(t)) => Self::Determined(t.clone()),
            (Self::Determined(s), Self::Indetermined) => Self::Determined(s.clone()),
            (Self::Determined(s), Self::Determined(t)) => Self::Determined(_Span {
                start: s.start.clone(),
                end: t.end.clone(),
            }),
        }
    }

    pub(super) fn till_block<R: Locatable>(&self, block: &Vec<R>) -> Self {
        match self {
            Self::Indetermined => {
                if block.is_empty() {
                    unreachable!()
                } else {
                    block.first().unwrap().span().till(block.last().unwrap())
                }
            }
            Self::Determined(s) => {
                let end = match block.last() {
                    Some(t) => match t.span() {
                        Self::Determined(u) => u.end.clone(),
                        Self::Indetermined => s.end.clone(),
                    },
                    None => s.end.clone(),
                };
                Self::Determined(_Span {
                    start: s.start.clone(),
                    end,
                })
            }
        }
    }

    pub(super) fn or<R: Locatable>(&self, other: &Option<R>) -> Span {
        match self {
            Self::Indetermined => match other {
                None => Self::Indetermined,
                Some(r) => match r.span() {
                    Self::Indetermined => Self::Indetermined,
                    Self::Determined(s) => Self::Determined(s.clone()),
                },
            },
            Self::Determined(s) => {
                let end = match other {
                    Some(t) => match t.span() {
                        Self::Indetermined => s.end.clone(),
                        Self::Determined(u) => u.end.clone(),
                    },
                    None => s.end.clone(),
                };
                Self::Determined(_Span {
                    start: s.start.clone(),
                    end,
                })
            }
        }
    }
}

impl _Span {
    fn till(&self, other: Self) -> Self {
        Self {
            start: self.start.clone(),
            end: other.end,
        }
    }
}

impl<R> Locatable for Vec<R>
where
    R: Locatable,
{
    fn span(&self) -> Span {
        if self.is_empty() {
            Span::Indetermined
        } else {
            let first = self.first().unwrap();
            let last = self.last().unwrap();
            let sp = match (first.span(), last.span()) {
                (Span::Indetermined, Span::Indetermined) => unreachable!(),
                (Span::Determined(s), Span::Indetermined) => s,
                (Span::Indetermined, Span::Determined(t)) => t,
                (Span::Determined(s), Span::Determined(t)) => s.till(t),
            };
            Span::Determined(sp)
        }
    }
}

impl Locatable for Token {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl<R> Locatable for Box<R>
where
    R: Locatable,
{
    fn span(&self) -> Span {
        (**self).span()
    }
}

impl Locatable for Span {
    fn span(&self) -> Span {
        self.clone()
    }
}

impl Locatable for Statement {
    fn span(&self) -> Span {
        match self {
            Self::FunctionDeclaration(_, _, s) => s.clone(),
            Self::Continue(s) => s.clone(),
            Self::Break(s) => s.clone(),
            Self::Pass(s) => s.clone(),
            Self::Expressions(_, s) => s.clone(),
            Self::Return(_, s) => s.clone(),
            Self::If(_, _, _, _, s) => s.clone(),
            Self::ClassDefinition(_, s) => s.clone(),
            Self::With(_, _, _, _, s) => s.clone(),
            Self::For(_, _, _, _, _, _, s) => s.clone(),
            Self::Try(_, _, _, _, s) => s.clone(),
            Self::While(_, _, _, s) => s.clone(),
            Self::Assignment(_, _, _, _, s) => s.clone(),
            Self::Del(_, s) => s.clone(),
            Self::Yield(_, s) => s.clone(),
            Self::Assert(_, _, s) => s.clone(),
            Self::Global(_, s) => s.clone(),
            Self::Nonlocal(_, s) => s.clone(),
            Self::Import(_, s) => s.clone(),
            Self::Raise(_, _, s) => s.clone(),
            Self::Match(_, _, s) => s.clone(),
            Self::Type(_, _, _, s) => s.clone(),
            Self::Invalid => unreachable!(),
        }
    }
}

impl Locatable for Name {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl Locatable for ClassDefinition {
    fn span(&self) -> Span {
        self.name.span().till(&self.body)
    }
}

impl Locatable for Decorator {
    fn span(&self) -> Span {
        self.0.span()
    }
}

impl Locatable for Expression {
    fn span(&self) -> Span {
        match self {
            Self::List(_, s) => s.clone(),
            Self::Case(_, _, _, s) => s.clone(),
            Self::Subscript(_, _, s) => s.clone(),
            Self::Call(_, _, s) => s.clone(),
            Self::UnaryOperation(_, _, s) => s.clone(),
            Self::Slice(_, _, s) => s.clone(),
            Self::WithItem(_, _, s) => s.clone(),
            Self::ExceptBlock(_, _, _, _, s) => s.clone(),
            Self::Walrus(_, _, s) => s.clone(),
            Self::Ternary(_, _, _, s) => s.clone(),
            Self::Comparison(_, _, s) => s.clone(),
            Self::Strings(_, s) => s.clone(),
            Self::Yield(_, s) => s.clone(),
            Self::YieldFrom(_, s) => s.clone(),
            Self::Generator(_, _, s) => s.clone(),
            Self::ForIfClause(_, _, _, _, s) => s.clone(),
            Self::BinaryOperation(_, _, s) => s.clone(),
            Self::ListComprehension(_, _, s) => s.clone(),
            Self::DictComprehension(_, _, s) => s.clone(),
            Self::SetComprehension(_, _, s) => s.clone(),
            Self::Lambda(_, _, s) => s.clone(),
            Self::Tuple(_, s) => s.clone(),
            Self::PrimaryGenexp(_, _, s) => s.clone(),
            Self::KeywordArgument(_, _, s) => s.clone(),
            Self::ListUnwrap(_, s) => s.clone(),
            Self::Name(_, s) => s.clone(),
            Self::Number(_, s) => s.clone(),
            Self::FStringReplacement(_, s) => s.clone(),
            Self::DictUnwrap(_, s) => s.clone(),
            Self::Dict(_, s) => s.clone(),
            Self::Set(_, s) => s.clone(),
            Self::TypeBound(_, s) => s.clone(),
            Self::Pattern(_, s) => s.clone(),
            Self::Attribute(_, s) => s.clone(),
            Self::TypeComment(_, s) => s.clone(),
            Self::None(s) => s.clone(),
            Self::Ellipsis(s) => s.clone(),
            Self::True(s) => s.clone(),
            Self::False(s) => s.clone(),
            Self::Invalid => unreachable!(),
        }
    }
}

impl Locatable for PyString {
    fn span(&self) -> Span {
        match self {
            Self::Literal(_, s) => s.clone(),
            Self::FString(vs) => vs.span(),
        }
    }
}

impl Locatable for FString {
    fn span(&self) -> Span {
        match self {
            Self::Literal(_, s) => s.clone(),
            Self::Interpolated(fs) => fs.span(),
        }
    }
}

impl Locatable for FStringReplacement {
    fn span(&self) -> Span {
        self.exprs.span().till_block(&self.format_specs)
    }
}

impl Locatable for Arguments {
    fn span(&self) -> Span {
        if self.positional.is_empty() && self.keyword.is_empty() {
            todo!()
        }
        if !self.positional.is_empty() {
            return self.positional.span().till_block(&self.keyword);
        }
        self.keyword.span()
    }
}

impl Locatable for Slice {
    fn span(&self) -> Span {
        match self {
            Self::Simple(expr) => expr.span(),
            Self::Delimited(l, m, r) => {
                l.clone().unwrap().span().or(&m).or(&r) // FIXME
            }
        }
    }
}
impl Locatable for Import {
    fn span(&self) -> Span {
        self.module.span().till_block(&self.items)
    }
}

impl Locatable for ImportItem {
    fn span(&self) -> Span {
        if self.name.is_empty() {
            Span::Indetermined
        } else {
            let last = self.name.last().unwrap();
            self.name.first().unwrap().span.till(last).or(&self.alias)
        }
    }
}

impl Locatable for Pattern {
    fn span(&self) -> Span {
        match self {
            Self::Wildcard => todo!(),
            Self::Capture(_pat, name) => name.span(),
            Self::Literal(expr) => expr.span(),
            Self::Value(vs) => vs.span(),
            Self::Group(pat) => pat.span(),
            Self::Sequence(seq) => seq.span(),
            Self::Star(pat) => pat.span(),
            Self::DoubleStar(name) => name.span(),
            Self::Mapping(ps) => ps.span(),
            Self::Class(expr, ps) => expr.span().till_block(ps),
            Self::Disjunction(ps) => ps.span(),
            Self::KeyValue(expr, pat) => expr.span().till(pat.as_ref()),
        }
    }
}

impl Locatable for Module {
    fn span(&self) -> Span {
        self.path.span().or(&self.alias)
    }
}

impl Locatable for FunctionDeclaration {
    fn span(&self) -> Span {
        self.name.span.till_block(&self.code)
    }
}
