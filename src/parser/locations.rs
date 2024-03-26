use super::ast::*;
use super::tokenizer::Token;
use std::rc::Rc;

#[derive(Clone, Copy, Default, Debug)]
pub(crate) struct Location {
    pub(crate) line: usize,
    pub(crate) column: usize,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.line + 1, self.column + 1)
    }
}

pub trait Locatable {
    fn span(&self) -> Span;
}

#[derive(Clone, Copy, Default)]
pub enum Span {
    #[default]
    Indetermined,
    Determined(_Span),
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Span::Determined(_Span { start, end }) => format!("{start}-{end}"),
            Span::Indetermined => "n/a".to_owned(),
        };
        write!(f, "{}", s)
    }
}

#[derive(Clone, Copy, Default, Debug)]
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
            (Self::Indetermined, Self::Determined(t)) => Self::Determined(t),
            (Self::Determined(s), Self::Indetermined) => Self::Determined(*s),
            (Self::Determined(s), Self::Determined(t)) => Self::Determined(_Span {
                start: s.start,
                end: t.end,
            }),
        }
    }

    pub(super) fn till_block<R: Locatable>(&self, block: &[R]) -> Self {
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
                        Self::Determined(u) => u.end,
                        Self::Indetermined => s.end,
                    },
                    None => s.end,
                };
                Self::Determined(_Span {
                    start: s.start,
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
                    Self::Determined(s) => Self::Determined(s),
                },
            },
            Self::Determined(s) => {
                let end = match other {
                    Some(t) => match t.span() {
                        Self::Indetermined => s.end,
                        Self::Determined(u) => u.end,
                    },
                    None => s.end,
                };
                Self::Determined(_Span {
                    start: s.start,
                    end,
                })
            }
        }
    }
}

impl _Span {
    fn till(&self, other: Self) -> Self {
        Self {
            start: self.start,
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
        self.span
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

impl<R> Locatable for Rc<R>
where
    R: Locatable,
{
    fn span(&self) -> Span {
        (**self).span()
    }
}

impl Locatable for Span {
    fn span(&self) -> Span {
        *self
    }
}

impl Locatable for Statement {
    fn span(&self) -> Span {
        match *self {
            Self::FunctionDeclaration(_, _, s) => s,
            Self::Continue(s) => s,
            Self::Break(s) => s,
            Self::Pass(s) => s,
            Self::Expressions(_, s) => s,
            Self::Return(_, s) => s,
            Self::If(_, _, _, _, s) => s,
            Self::ClassDefinition(_, s) => s,
            Self::With(_, _, _, _, s) => s,
            Self::For(_, _, _, _, _, _, s) => s,
            Self::Try(_, _, _, _, s) => s,
            Self::While(_, _, _, s) => s,
            Self::Assignment(_, _, _, _, s) => s,
            Self::Del(_, s) => s,
            Self::Yield(_, s) => s,
            Self::Assert(_, _, s) => s,
            Self::Global(_, s) => s,
            Self::Nonlocal(_, s) => s,
            Self::Import(_, s) => s,
            Self::Raise(_, _, s) => s,
            Self::Match(_, _, s) => s,
            Self::Type(_, _, _, s) => s,
            Self::Invalid(s) => s,
        }
    }
}

impl Locatable for Name {
    fn span(&self) -> Span {
        self.span
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
        match *self {
            Self::List(_, s) => s,
            Self::Case(_, _, _, s) => s,
            Self::Subscript(_, _, s) => s,
            Self::Call(_, _, s) => s,
            Self::UnaryOperation(_, _, s) => s,
            Self::Slice(_, _, s) => s,
            Self::WithItem(_, _, s) => s,
            Self::ExceptBlock(_, _, _, _, s) => s,
            Self::Walrus(_, _, s) => s,
            Self::Ternary(_, _, _, s) => s,
            Self::Comparison(_, _, s) => s,
            Self::Strings(_, s) => s,
            Self::Yield(_, s) => s,
            Self::YieldFrom(_, s) => s,
            Self::Generator(_, _, s) => s,
            Self::ForIfClause(_, _, _, _, s) => s,
            Self::BinaryOperation(_, _, _, s) => s,
            Self::ListComprehension(_, _, s) => s,
            Self::DictComprehension(_, _, s) => s,
            Self::SetComprehension(_, _, s) => s,
            Self::Lambda(_, _, s) => s,
            Self::Tuple(_, s) => s,
            Self::KeywordArgument(_, _, s) => s,
            Self::ListUnwrap(_, s) => s,
            Self::Name(_, s) => s,
            Self::Number(_, s) => s,
            Self::FStringReplacement(_, s) => s,
            Self::DictUnwrap(_, s) => s,
            Self::Dict(_, s) => s,
            Self::Set(_, s) => s,
            Self::TypeBound(_, s) => s,
            Self::Pattern(_, s) => s,
            Self::Attribute(_, s) => s,
            Self::TypeComment(_, s) => s,
            Self::None(s) => s,
            Self::Ellipsis(s) => s,
            Self::True(s) => s,
            Self::False(s) => s,
            Self::FString(_, s) => s,
            Self::ImportItems(_, s) => s,
            Self::Parameters(_, s) => s,
            Self::Arguments(_, s) => s,
            Self::Invalid(s) => s,
        }
    }
}

impl Locatable for PyString {
    fn span(&self) -> Span {
        match self {
            Self::Literal(_, s) => *s,
            Self::FString(vs) => vs.span(),
        }
    }
}

impl Locatable for FString {
    fn span(&self) -> Span {
        match self {
            Self::Literal(_, s) => *s,
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
            return Span::Indetermined;
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
                if l.is_some() {
                    l.as_ref().unwrap().span().or(m).or(r)
                } else if m.is_some() {
                    m.as_ref().unwrap().span().or(r)
                } else if r.is_some() {
                    r.as_ref().unwrap().span()
                } else {
                    Span::Indetermined
                }
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
        self.name.span
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
            Self::Invalid(s) => *s,
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

impl Locatable for Parameter {
    fn span(&self) -> Span {
        self.name.span
    }
}
