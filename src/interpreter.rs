use crate::parser::Expression;

use super::parser::Statement;

pub enum FlowControl {
    NextStatement,
    Break,
    Continue,
    Return(Box<dyn PyObject>),
    Raise(Box<dyn PyObject>),
}

pub fn evaluate(statement: &Statement) -> FlowControl {
    match statement {
        Statement::FunctionDeclaration(decl, decorators, _) => todo!(),
        Statement::Continue(_) => FlowControl::Continue,
        Statement::Break(_) => FlowControl::Break,
        Statement::Pass(_) => FlowControl::NextStatement,
        Statement::Expressions(exprs, _) => {
            for expr in exprs {
                evaluate_expr(expr);
            }
            FlowControl::NextStatement
        }
        Statement::Return(exprs, _) => todo!(),
        Statement::If(expr, then_block, elifs, else_block, _) => {
            if evaluate_expr(expr).__bool__() {
                for stmt in then_block {
                    let flow = evaluate(stmt);
                    if !matches!(flow, FlowControl::NextStatement) {
                        return flow;
                    }
                }
                return FlowControl::NextStatement;
            }
            for (elif, block) in elifs {
                if evaluate_expr(elif).__bool__() {
                    for stmt in block {
                        let flow = evaluate(stmt);
                        if !matches!(flow, FlowControl::NextStatement) {
                            return flow;
                        }
                    }
                    return FlowControl::NextStatement;
                }
            }
            if let Some(else_block) = else_block {
                for stmt in else_block {
                    let flow = evaluate(stmt);
                    if !matches!(flow, FlowControl::NextStatement) {
                        return flow;
                    }
                }
            }
            FlowControl::NextStatement
        }
        Statement::ClassDefinition(class, _) => todo!(),
        Statement::With(expr, block, type_comment, is_async, _) => todo!(),
        Statement::For(tgts, exprs, block, else_block, type_comment, is_async, _) => todo!(),
        Statement::Try(block, except_block, else_block, finally_block, _) => todo!(),
        Statement::While(expr, block, else_block, _) => {
            while evaluate_expr(expr).__bool__() {
                for stmt in block {
                    let flow = evaluate(stmt);
                    match flow {
                        FlowControl::Continue => break,
                        FlowControl::Break => return FlowControl::NextStatement,
                        FlowControl::Return(v) => return FlowControl::Return(v),
                        FlowControl::Raise(e) => return FlowControl::Raise(e),
                        _ => {}
                    }
                }
            }
            if let Some(else_block) = else_block {
                for stmt in else_block {
                    let flow = evaluate(stmt);
                    if !matches!(flow, FlowControl::NextStatement) {
                        return flow;
                    }
                }
            }
            FlowControl::NextStatement
        }
        Statement::Assignment(tgts, op, rhs, typ, _) => todo!(),
        Statement::Del(exprs, _) => todo!(),
        Statement::Yield(expr, _) => todo!(),
        Statement::Assert(expr, msg, _) => todo!(),
        Statement::Global(names, _) => todo!(),
        Statement::Nonlocal(names, _) => todo!(),
        Statement::Import(imports, _) => todo!(),
        Statement::Raise(expr, from_expr, _) => todo!(),
        Statement::Match(expr, cases, _) => todo!(),
        Statement::Type(name, type_params, expr, _) => todo!(),
        Statement::Invalid(_) => unreachable!(),
    }
}

fn evaluate_expr(expression: &Expression) -> impl PyObject {
    match expression {
        Expression::ListUnwrap(expr, _) => todo!(),
        Expression::BinaryOperation(op, larg, rarg, _) => todo!(),
        Expression::UnaryOperation(op, arg, _) => todo!(),
        Expression::Subscript(expr, name, _) => todo!(),
        Expression::Call(expr, args, _) => todo!(),
        Expression::Slice(expr, slices, _) => todo!(),
        Expression::WithItem(expr, as_expr, _) => todo!(),
        Expression::ExceptBlock(expr, capture, block, starred, _) => todo!(),
        Expression::Walrus(name, expr, _) => todo!(),
        Expression::Ternary(expr, then_expr, else_expr, _) => todo!(),
        Expression::Comparison(lhs, rhs, _) => todo!(),
        Expression::Strings(strings, _) => todo!(),
        Expression::Yield(expr, _) => todo!(),
        Expression::YieldFrom(generator, _) => todo!(),
        Expression::Generator(elem, for_ifs, _) => todo!(),
        Expression::ForIfClause(tgts, set, ifs, is_async, _) => todo!(),
        Expression::Tuple(elements, _) => todo!(),
        Expression::List(elements, _) => todo!(),
        Expression::ListComprehension(elem, for_ifs, _) => todo!(),
        Expression::DictUnwrap(dict, _) => todo!(),
        Expression::Dict(items, _) => todo!(),
        Expression::DictComprehension(elem, for_ifs, _) => todo!(),
        Expression::Set(items, _) => todo!(),
        Expression::SetComprehension(elem, for_ifs, _) => todo!(),
        Expression::KeywordArgument(name, value, _) => todo!(),
        Expression::FStringReplacement(expr, _) => todo!(),
        Expression::Name(name, _) => todo!(),
        Expression::Number(num, _) => todo!(),
        Expression::Ellipsis(_) => (),
        Expression::True(_) => todo!(),
        Expression::False(_) => todo!(),
        Expression::None(_) => todo!(),
        Expression::Case(pattern, capture, block, _) => todo!(),
        Expression::TypeBound(typ, _) => todo!(),
        Expression::Pattern(pattern, _) => todo!(),
        Expression::Attribute(attr, _) => todo!(),
        Expression::Lambda(params, expr, _) => todo!(),
        Expression::TypeComment(type_comment, _) => todo!(),
        Expression::FString(_, _) => todo!(),
        Expression::ImportItems(_, _) => unreachable!(),
        Expression::Parameters(_, _) => unreachable!(),
        Expression::Arguments(_, _) => unreachable!(),
        Expression::Invalid(_) => unreachable!(),
    }
}

pub trait PyObject {
    fn __bool__(&self) -> bool;
}

impl PyObject for () {
    fn __bool__(&self) -> bool {
        false
    }
}
