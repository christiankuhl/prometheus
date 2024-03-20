// # ========================= START OF THE GRAMMAR =========================

// # General grammatical elements and rules:
// #
// # * Strings with double quotes (") denote SOFT KEYWORDS
// # * Strings with single quotes (') denote KEYWORDS
// # * Upper case names (NAME) denote tokens in the Grammar/Tokens file
// # * Rule names starting with "invalid_" are used for specialized syntax errors
// #     - These rules are NOT used in the first pass of the parser.
// #     - Only if the first pass fails to parse, a second pass including the invalid
// #       rules will be executed.
// #     - If the parser fails in the second phase with a generic syntax error, the
// #       location of the generic failure of the first pass will be used (this avoids
// #       reporting incorrect locations due to the invalid rules).
// #     - The order of the alternatives involving invalid rules matter
// #       (like any rule in PEG).
// #
// # Grammar Syntax (see PEP 617 for more information):
// #
// # rule_name: expression
// #   Optionally, a type can be included right after the rule name, which
// #   specifies the return type of the C or Python function corresponding to the
// #   rule:
// # rule_name[return_type]: expression
// #   If the return type is omitted, then a void * is returned in C and an Any in
// #   Python.
// # e1 e2
// #   Match e1, then match e2.
// # e1 | e2
// #   Match e1 or e2.
// #   The first alternative can also appear on the line after the rule name for
// #   formatting purposes. In that case, a | must be used before the first
// #   alternative, like so:
// #       rule_name[return_type]:
// #            | first_alt
// #            | second_alt
// # ( e )
// #   Match e (allows also to use other operators in the group like '(e)*')
// # [ e ] or e?
// #   Optionally match e.
// # e*
// #   Match zero or more occurrences of e.
// # e+
// #   Match one or more occurrences of e.
// # s.e+
// #   Match one or more occurrences of e, separated by s. The generated parse tree
// #   does not include the separator. This is otherwise identical to (e (s e)*).
// # &e
// #   Succeed if e can be parsed, without consuming any input.
// # !e
// #   Fail if e can be parsed, without consuming any input.
// # ~
// #   Commit to the current alternative, even if it fails to parse.
// #

use std::cell::RefCell;
use std::rc::Rc;

use super::ast::*;
use super::combinators::*;
use super::error::Error;
use super::locations::Locatable;
use super::memo::{save_result, try_remember};
use super::tokenizer::{Token, TokenType as TT};

pub fn parse(input: &[Token]) -> (Vec<Statement>, Vec<Error>) {
    let errors = RefCell::new(Vec::new());
    let cache = RefCell::new(ExpressionCache::new());
    let parser_input = ParserInput::new(input, ParserState::new(&errors, &cache), Pass::FirstScan);
    let stmts = match file_.parse(parser_input) {
        ParseResult::Err => {
            let cache = RefCell::new(ExpressionCache::new());
            let parser_input = ParserInput::new(
                input,
                ParserState::new(&errors, &cache),
                Pass::ErrorLocation,
            );
            let res = file_.parse(parser_input);
            println!("{:?}", res);
            vec![]
        }
        ParseResult::Ok((stmts, _)) => stmts,
    };
    (stmts, errors.into_inner())
}

pub fn parse_interactive(input: &[Token]) -> (Vec<Statement>, Vec<Error>) {
    let errors = RefCell::new(Vec::new());
    let cache = RefCell::new(ExpressionCache::new());
    let input = ParserInput::new(input, ParserState::new(&errors, &cache), Pass::FirstScan);
    let stmts = interactive.parse(input).expect("Parser shall not fail.");
    (stmts, errors.into_inner())
}

// # STARTING RULES
// # ==============
// file: [statements] ENDMARKER
fn file_(input: ParserInput) -> ParseResult<Vec<Statement>> {
    left(maybe(statements), tok(TT::ENDMARKER))
        .map(|v| v.unwrap_or_default())
        .parse(input)
}

// interactive: statement_newline
fn interactive(input: ParserInput) -> ParseResult<Vec<Statement>> {
    statement_newline.parse(input)
}

// eval: expressions NEWLINE* ENDMARKER
fn eval(input: ParserInput) -> ParseResult<Vec<Rc<Expression>>> {
    left(
        expressions,
        pair(zero_or_more(tok(TT::NEWLINE)), tok(TT::ENDMARKER)),
    )
    .parse(input)
}

// func_type: '(' [type_expressions] ')' '->' expression NEWLINE* ENDMARKER

// # GENERAL STATEMENTS
// # ==================

fn pass(input: ParserInput) -> ParseResult<Statement> {
    token_nodiscard(TT::KEYWORD, "pass")
        .map(|t| Statement::Pass(t.span))
        .parse(input)
}

fn break_(input: ParserInput) -> ParseResult<Statement> {
    token_nodiscard(TT::KEYWORD, "break")
        .map(|t| Statement::Break(t.span))
        .parse(input)
}

fn continue_(input: ParserInput) -> ParseResult<Statement> {
    token_nodiscard(TT::KEYWORD, "continue")
        .map(|t| Statement::Continue(t.span))
        .parse(input)
}

// statements: statement+
fn statements(input: ParserInput) -> ParseResult<Vec<Statement>> {
    one_or_more(statement).map(|s| s.concat()).parse(input)
}

// statement: compound_stmt  | simple_stmts
fn statement(input: ParserInput) -> ParseResult<Vec<Statement>> {
    compound_stmt
        .map(|cs| vec![cs])
        .or(simple_stmts)
        .parse(input)
}
// statement_newline:
//     | compound_stmt NEWLINE
//     | simple_stmts
//     | NEWLINE
//     | ENDMARKER
fn statement_newline(input: ParserInput) -> ParseResult<Vec<Statement>> {
    left(compound_stmt, tok(TT::NEWLINE))
        .map(|_| vec![])
        .or(simple_stmts)
        .or(tok(TT::NEWLINE).map(|_| vec![]))
        .or(tok(TT::ENDMARKER).map(|_| vec![]))
        .parse(input)
}

// simple_stmts:
//     | simple_stmt !';' NEWLINE  # Not needed, there for speedup
//     | ';'.simple_stmt+ [';'] NEWLINE
fn simple_stmts(input: ParserInput) -> ParseResult<Vec<Statement>> {
    left(simple_stmt, pair(not(tok(TT::SEMI)), tok(TT::NEWLINE)))
        .map(|s| vec![s])
        .or(left(
            right(tok(TT::SEMI), sep_by(simple_stmt, TT::SEMI)),
            pair(maybe(tok(TT::SEMI)), tok(TT::NEWLINE)),
        ))
        .parse(input)
}

// # NOTE: assignment MUST precede expression, else parsing a simple assignment
// # will throw a SyntaxError.
// simple_stmt:
//     | assignment
//     | type_alias
//     | star_expressions
//     | return_stmt
//     | import_stmt
//     | raise_stmt
//     | 'pass'
//     | del_stmt
//     | yield_stmt
//     | assert_stmt
//     | 'break'
//     | 'continue'
//     | global_stmt
//     | nonlocal_stmt
fn simple_stmt(input: ParserInput) -> ParseResult<Statement> {
    assignment
        .or(type_alias)
        .or(star_expressions.map(|e| {
            let s = e.span();
            Statement::Expressions(e, s)
        }))
        .or(return_stmt)
        .or(import_stmt)
        .or(raise_stmt)
        .or(pass)
        .or(del_stmt)
        .or(yield_stmt)
        .or(assert_stmt)
        .or(break_)
        .or(continue_)
        .or(global_stmt)
        .or(nonlocal_stmt)
        .parse(input)
}

// compound_stmt:
//     | function_def
//     | if_stmt
//     | class_def
//     | with_stmt
//     | for_stmt
//     | try_stmt
//     | while_stmt
//     | match_stmt
fn compound_stmt(input: ParserInput) -> ParseResult<Statement> {
    on_error_pass(invalid_compound_stmt)
        .or(function_def)
        .or(if_stmt)
        .or(class_def)
        .or(with_stmt)
        .or(for_stmt)
        .or(try_stmt)
        .or(while_stmt)
        .or(match_stmt)
        .parse(input)
}

// # SIMPLE STATEMENTS
// # =================

// # NOTE: annotated_rhs may start with 'yield'; yield_expr must start with 'yield'
// assignment:
//     | NAME ':' expression ['=' annotated_rhs ]
//     | ('(' single_target ')'
//          | single_subscript_attribute_target) ':' expression ['=' annotated_rhs ]
//     | (star_targets '=' )+ (yield_expr | star_expressions) !'=' [TYPE_COMMENT]
//     | single_target augassign ~ (yield_expr | star_expressions)
fn assignment(input: ParserInput) -> ParseResult<Statement> {
    pair(
        pair(left(name, tok(TT::COLON)), expression),
        maybe(right(tok(TT::EQUAL), annotated_rhs)),
    )
    .map(|((n, t), r)| {
        let s = n.span().till(&t).or(&r);
        let ns = n.span();
        Statement::Assignment(vec![Rc::new(Expression::Name(n, ns))], None, r, Some(t), s)
    })
    .or(pair(
        pair(
            right(tok(TT::LPAR), left(single_target, tok(TT::RPAR)))
                .or(single_subscript_attribute_target),
            right(tok(TT::COLON), expression),
        ),
        maybe(right(tok(TT::EQUAL), annotated_rhs)),
    )
    .map(|((l, t), r)| {
        let s = t.span().or(&r);
        Statement::Assignment(vec![l], None, r, Some(t), s)
    }))
    .or(pair(
        pair(
            one_or_more(left(star_targets, tok(TT::EQUAL))),
            yield_expr.map(|e| vec![e]).or(star_expressions),
        ),
        right(not(tok(TT::EQUAL)), maybe(tok(TT::TYPE_COMMENT))),
    )
    .map(|((l, r), t)| {
        let s = r.span().or(&t);
        Statement::Assignment(
            l.concat(),
            None,
            Some(r),
            t.map(|s| Rc::new(Expression::TypeComment(Rc::from(s.lexeme), s.span))),
            s,
        )
    }))
    .or(pair(
        pair(single_target, augassign),
        yield_expr.map(|e| vec![e]).or(star_expressions),
    )
    .map(|((l, ref o), r)| {
        let s = l.span().till(&r);
        Statement::Assignment(vec![l], Some(o.into()), Some(r), None, s)
    }))
    .or(on_error_pass(invalid_assignment))
    .parse(input)
}

// annotated_rhs: yield_expr | star_expressions
fn annotated_rhs(input: ParserInput) -> ParseResult<Vec<Rc<Expression>>> {
    yield_expr
        .map(|e| vec![e])
        .or(star_expressions)
        .parse(input)
}

// augassign:
//     | '+='
//     | '-='
//     | '*='
//     | '@='
//     | '/='
//     | '%='
//     | '&='
//     | '|='
//     | '^='
//     | '<<='
//     | '>>='
//     | '**='
//     | '//='
fn augassign(input: ParserInput) -> ParseResult<Token> {
    tok(TT::PLUSEQUAL)
        .or(tok(TT::MINEQUAL))
        .or(tok(TT::STAREQUAL))
        .or(tok(TT::ATEQUAL))
        .or(tok(TT::SLASHEQUAL))
        .or(tok(TT::PERCENTEQUAL))
        .or(tok(TT::AMPEREQUAL))
        .or(tok(TT::VBAREQUAL))
        .or(tok(TT::CIRCUMFLEXEQUAL))
        .or(tok(TT::LEFTSHIFTEQUAL))
        .or(tok(TT::RIGHTSHIFTEQUAL))
        .or(tok(TT::DOUBLESTAREQUAL))
        .or(tok(TT::DOUBLESLASHEQUAL))
        .parse(input)
}

// return_stmt:
//     | 'return' [star_expressions]
fn return_stmt(input: ParserInput) -> ParseResult<Statement> {
    pair(token_nodiscard(TT::KEYWORD, "return"), star_expressions)
        .map(|(r, e)| {
            let s = if e.is_empty() {
                r.span
            } else {
                r.span.till(&e)
            };
            Statement::Return(e, s)
        })
        .parse(input)
}

// raise_stmt:
//     | 'raise' expression ['from' expression ]
//     | 'raise'
fn raise_stmt(input: ParserInput) -> ParseResult<Statement> {
    pair(
        right(token(TT::KEYWORD, "raise"), expression),
        maybe(right(token(TT::KEYWORD, "from"), expression)),
    )
    .map(|(e, f)| {
        let s = e.span().or(&f);
        Statement::Raise(Some(e), f, s)
    })
    .or(token_nodiscard(TT::KEYWORD, "raise").map(|t| Statement::Raise(None, None, t.span)))
    .parse(input)
}

// global_stmt: 'global' ','.NAME+
fn global_stmt(input: ParserInput) -> ParseResult<Statement> {
    pair(
        token_nodiscard(TT::KEYWORD, "global"),
        sep_by(name, TT::COMMA),
    )
    .map(|(t, ns)| {
        let s = t.span.till(&ns);
        Statement::Global(ns, s)
    })
    .parse(input)
}

// nonlocal_stmt: 'nonlocal' ','.NAME+
fn nonlocal_stmt(input: ParserInput) -> ParseResult<Statement> {
    pair(
        token_nodiscard(TT::KEYWORD, "nonlocal"),
        sep_by(name, TT::COMMA),
    )
    .map(|(n, ns)| {
        let s = n.span.till(&ns);
        Statement::Nonlocal(ns, s)
    })
    .parse(input)
}

// del_stmt:
//     | 'del' del_targets &(';' | NEWLINE)
fn del_stmt(input: ParserInput) -> ParseResult<Statement> {
    left(
        pair(token_nodiscard(TT::KEYWORD, "del"), del_targets),
        lookahead(tok(TT::SEMI).or(tok(TT::NEWLINE))),
    )
    .map(|(t, ns)| {
        let span = t.span.till_block(&ns);
        Statement::Del(ns, span)
    })
    .or(on_error_pass(invalid_del_stmt))
    .parse(input)
}

// yield_stmt: yield_expr
fn yield_stmt(input: ParserInput) -> ParseResult<Statement> {
    yield_expr
        .map(|e| {
            let s = e.span();
            Statement::Yield(e, s)
        })
        .parse(input)
}

// assert_stmt: 'assert' expression [',' expression ]
fn assert_stmt(input: ParserInput) -> ParseResult<Statement> {
    pair(
        token_nodiscard(TT::KEYWORD, "assert"),
        pair(expression, maybe(right(tok(TT::COMMA), expression))),
    )
    .map(|(t, (e, m))| {
        let s = t.span.till(&e).or(&m);
        Statement::Assert(e, m, s)
    })
    .parse(input)
}

// import_stmt:
//     | import_name
//     | import_from
fn import_stmt(input: ParserInput) -> ParseResult<Statement> {
    on_error_pass(invalid_import)
        .or(import_name.or(import_from))
        .parse(input)
}

// # Import statements
// # -----------------

// import_name: 'import' dotted_as_names
fn import_name(input: ParserInput) -> ParseResult<Statement> {
    right(token(TT::KEYWORD, "import"), dotted_as_names)
        .map(|ms| {
            let imports: Vec<Import> = ms
                .iter()
                .map(|m| Import {
                    module: m.clone(),
                    items: vec![],
                })
                .collect();
            let span = imports.span();
            Statement::Import(imports, span)
        })
        .parse(input)
}

fn import_level(input: ParserInput) -> ParseResult<usize> {
    zero_or_more(tok(TT::DOT).or(tok(TT::ELLIPSIS)))
        .map(count_dots)
        .parse(input)
}

fn import_level_at_least_one(input: ParserInput) -> ParseResult<usize> {
    one_or_more(tok(TT::DOT).or(tok(TT::ELLIPSIS)))
        .map(count_dots)
        .parse(input)
}

fn count_dots(tokens: Vec<Token>) -> usize {
    tokens
        .iter()
        .map(|t| match t.typ {
            TT::DOT => 1,
            TT::ELLIPSIS => 3,
            _ => 0,
        })
        .sum()
}

// # note below: the ('.' | '...') is necessary because '...' is tokenized as ELLIPSIS
// import_from:
//     | 'from' ('.' | '...')* dotted_name 'import' import_from_targets
//     | 'from' ('.' | '...')+ 'import' import_from_targets
fn import_from(input: ParserInput) -> ParseResult<Statement> {
    pair(
        right(token(TT::KEYWORD, "from"), pair(import_level, dotted_name)),
        right(token(TT::KEYWORD, "import"), import_from_targets),
    )
    .or(pair(
        right(token(TT::KEYWORD, "from"), import_level_at_least_one),
        right(token(TT::KEYWORD, "import"), import_from_targets),
    )
    .map(|(n, t)| ((n, vec![]), t)))
    .map(|((rel_level, path), items)| {
        let (items, span) = match items.as_ref() {
            Expression::ImportItems(items, span) => (items, span),
            _ => return Statement::Invalid,
        };
        Statement::Import(
            vec![Import {
                module: Module {
                    rel_level,
                    path,
                    alias: None,
                },
                items: items.clone(),
            }],
            span.clone(),
        )
    })
    .parse(input)
}

// import_from_targets:
//     | '(' import_from_as_names [','] ')'
//     | import_from_as_names !','
//     | '*'
fn import_from_targets(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(
        right(tok(TT::LPAR), import_from_as_names),
        pair(maybe(tok(TT::COMMA)), tok(TT::RPAR)),
    )
    .or(left(import_from_as_names, not(tok(TT::COMMA))))
    .or(tok(TT::STAR).map(|a| {
        Rc::new(Expression::ImportItems(
            vec![ImportItem {
                name: vec![],
                alias: None,
            }],
            a.span(),
        ))
    }))
    .or(on_error_pass(invalid_import_from_targets))
    .parse(input)
}

// import_from_as_names:
//     | ','.import_from_as_name+
fn import_from_as_names(input: ParserInput) -> ParseResult<Rc<Expression>> {
    sep_by(import_from_as_name, TT::COMMA)
        .map(|ns| {
            let s = ns.span();
            Rc::new(Expression::ImportItems(ns, s))
        })
        .parse(input)
}

// import_from_as_name:
//     | NAME ['as' NAME ]
fn import_from_as_name(input: ParserInput) -> ParseResult<ImportItem> {
    pair(name, maybe(right(token(TT::KEYWORD, "as"), name)))
        .map(|(n, alias)| ImportItem {
            name: vec![n],
            alias,
        })
        .parse(input)
}

// dotted_as_names:
//     | ','.dotted_as_name+
fn dotted_as_names(input: ParserInput) -> ParseResult<Vec<Module>> {
    sep_by(dotted_as_name, TT::COMMA).parse(input)
}

// dotted_as_name:
//     | dotted_name ['as' NAME ]
fn dotted_as_name(input: ParserInput) -> ParseResult<Module> {
    pair(dotted_name, maybe(right(token(TT::KEYWORD, "as"), name)))
        .map(|(path, alias)| Module {
            rel_level: 0,
            path,
            alias,
        })
        .parse(input)
}

// dotted_name:
//     | dotted_name '.' NAME
//     | NAME
fn dotted_name(input: ParserInput) -> ParseResult<Vec<Name>> {
    sep_by(name, TT::DOT).parse(input)
}

// # COMPOUND STATEMENTS
// # ===================

// # Common elements
// # ---------------

// block:
//     | NEWLINE INDENT statements DEDENT
//     | simple_stmts
fn block(input: ParserInput) -> ParseResult<Vec<Statement>> {
    left(
        right(pair(tok(TT::NEWLINE), tok(TT::INDENT)), statements),
        tok(TT::DEDENT),
    )
    .or(simple_stmts)
    .or(on_error_pass(invalid_block))
    .parse(input)
}

// decorators: ('@' named_expression NEWLINE )*
fn decorators(input: ParserInput) -> ParseResult<Vec<Decorator>> {
    zero_or_more(right(tok(TT::AT), left(named_expression, tok(TT::NEWLINE))).map(Decorator))
        .parse(input)
}

// # Class definitions
// # -----------------

// class_def:
//     | decorators class_def_raw
fn class_def(input: ParserInput) -> ParseResult<Statement> {
    pair(decorators, class_def_raw)
        .map(|(dec, mut cls)| {
            match cls {
                Statement::ClassDefinition(ref mut c, ref mut s) => {
                    *s = dec.span().till(c);
                    c.decorators = dec;
                }
                _ => {}
            };
            cls
        })
        .parse(input)
}

// class_def_raw:
//     | 'class' NAME [type_params] ['(' [arguments] ')' ] ':' block
fn class_def_raw(input: ParserInput) -> ParseResult<Statement> {
    on_error_pass(invalid_class_def_raw)
        .or(pair(
            right(token(TT::KEYWORD, "class"), pair(name, maybe(type_params))),
            pair(
                maybe(right(tok(TT::LPAR), left(arguments, tok(TT::RPAR)))),
                right(tok(TT::COLON), block),
            ),
        )
        .map(|((name, ts), (ancestors, body))| {
            let span = name.span().till_block(&body);
            let ancestors = match ancestors.as_deref() {
                Some(Expression::Arguments(args, _)) => args.clone(),
                None => Arguments::empty(),
                _ => return Statement::Invalid,
            };
            Statement::ClassDefinition(
                ClassDefinition {
                    name,
                    ancestors,
                    body,
                    decorators: vec![],
                    type_params: ts.unwrap_or_default(),
                },
                span,
            )
        }))
        .parse(input)
}

// # Function definitions
// # --------------------

// function_def:
//     | decorators function_def_raw
fn function_def(input: ParserInput) -> ParseResult<Statement> {
    pair(decorators, function_def_raw)
        .map(|(dec, mut fun)| {
            match fun {
                Statement::FunctionDeclaration(ref f, ref mut d, ref mut s) => {
                    *s = dec.span().till(f);
                    (*d).extend(dec);
                }
                _ => {}
            }
            fun
        })
        .parse(input)
}

// function_def_raw:
//     | 'def' NAME [type_params] '(' [params] ')' ['->' expression ] ':' [func_type_comment] block
//     | ASYNC 'def' NAME [type_params] '(' [params] ')' ['->' expression ] ':' [func_type_comment] block
fn function_def_raw(input: ParserInput) -> ParseResult<Statement> {
    on_error_pass(invalid_def_raw)
        .or(pair(
            maybe(token(TT::KEYWORD, "async")),
            pair(
                pair(
                    right(token(TT::KEYWORD, "def"), name),
                    right(
                        tok(TT::LPAR),
                        left(params, pair(tok(TT::RPAR), tok(TT::COLON))),
                    ),
                ),
                block,
            ),
        )
        .map(|(a, ((n, p), b))| {
            let span = n.span().till_block(&b);
            let parameters = match p.as_ref() {
                Expression::Parameters(p, _) => p.clone(),
                _ => return Statement::Invalid,
            };
            Statement::FunctionDeclaration(
                FunctionDeclaration {
                    name: n,
                    parameters,
                    code: b,
                    is_async: a.is_some(),
                },
                vec![],
                span,
            )
        }))
        .parse(input)
}

// # Function parameters
// # -------------------
fn name(input: ParserInput) -> ParseResult<Name> {
    tok(TT::NAME).parse(input).into()
}

// params:
//     | parameters
fn params(input: ParserInput) -> ParseResult<Rc<Expression>> {
    on_error_pass(invalid_parameters)
        .or(parameters.map(|p| {
            let s = p.span();
            Rc::new(Expression::Parameters(p, s))
        }))
        .parse(input)
}

// parameters:
//     | slash_no_default param_no_default* param_with_default* [star_etc]
//     | slash_with_default param_with_default* [star_etc]
//     | param_no_default+ param_with_default* [star_etc]
//     | param_with_default+ [star_etc]
//     | star_etc
fn parameters(input: ParserInput) -> ParseResult<Vec<Parameter>> {
    pair(
        pair(slash_no_default, zero_or_more(param_no_default)),
        pair(zero_or_more(param_with_default), maybe(star_etc)),
    )
    .map(|((mut u, v), (w, o))| {
        u.extend(v);
        u.extend(w);
        u.extend(o.unwrap_or_default());
        u
    })
    .or(pair(
        pair(slash_with_default, zero_or_more(param_with_default)),
        maybe(star_etc),
    )
    .map(|((mut u, v), o)| {
        u.extend(v);
        u.extend(o.unwrap_or_default());
        u
    }))
    .or(pair(
        pair(
            one_or_more(param_no_default),
            zero_or_more(param_with_default),
        ),
        maybe(star_etc),
    )
    .map(|((mut u, v), o)| {
        u.extend(v);
        u.extend(o.unwrap_or_default());
        u
    }))
    .or(
        pair(one_or_more(param_with_default), maybe(star_etc)).map(|(mut u, o)| {
            u.extend(o.unwrap_or_default());
            u
        }),
    )
    .or(star_etc)
    .parse(input)
}

// # Some duplication here because we can't write (',' | &')'),
// # which is because we don't support empty alternatives (yet).

// slash_no_default:
//     | param_no_default+ '/' ','
//     | param_no_default+ '/' &')'
fn slash_no_default(input: ParserInput) -> ParseResult<Vec<Parameter>> {
    left(
        one_or_more(param_no_default),
        pair(tok(TT::SLASH), tok(TT::COMMA)),
    )
    .or(left(
        one_or_more(param_no_default),
        pair(tok(TT::SLASH), lookahead(tok(TT::RPAR))),
    ))
    .parse(input)
}

// slash_with_default:
//     | param_no_default* param_with_default+ '/' ','
//     | param_no_default* param_with_default+ '/' &')'
fn slash_with_default(input: ParserInput) -> ParseResult<Vec<Parameter>> {
    pair(
        zero_or_more(param_no_default),
        left(
            one_or_more(param_with_default),
            pair(tok(TT::SLASH), tok(TT::COMMA)),
        ),
    )
    .or(pair(
        zero_or_more(param_no_default),
        left(
            one_or_more(param_with_default),
            pair(tok(TT::SLASH), lookahead(tok(TT::RPAR))),
        ),
    ))
    .map(|(mut u, v)| {
        u.extend(v);
        u
    })
    .parse(input)
}

// star_etc:
//     | '*' param_no_default param_maybe_default* [kwds]
//     | '*' param_no_default_star_annotation param_maybe_default* [kwds]
//     | '*' ',' param_maybe_default+ [kwds]
//     | kwds
fn star_etc(input: ParserInput) -> ParseResult<Vec<Parameter>> {
    on_error_pass(invalid_star_etc.map(|_| vec![]))
        .or(pair(
            right(tok(TT::STAR), param_no_default),
            pair(zero_or_more(param_maybe_default), maybe(kwds)),
        )
        .map(|(v, (mut u, w))| {
            u.push(v);
            if let Some(Expression::Parameters(p, _)) = w.as_deref() {
                u.extend(p.clone())
            };
            u
        }))
        .or(pair(
            right(tok(TT::STAR), param_no_default_star_annotation),
            pair(zero_or_more(param_maybe_default), maybe(kwds)),
        )
        .map(|(v, (mut u, w))| {
            u.push(v);
            if let Some(Expression::Parameters(p, _)) = w.as_deref() {
                u.extend(p.clone())
            };
            u
        }))
        .or(pair(
            right(
                pair(tok(TT::STAR), tok(TT::COMMA)),
                one_or_more(param_maybe_default),
            ),
            maybe(kwds),
        )
        .map(|(mut u, v)| {
            if let Some(Expression::Parameters(p, _)) = v.as_deref() {
                u.extend(p.clone())
            };
            u
        }))
        .or(kwds.map(|_| vec![]))
        .parse(input)
}

// kwds:
//     | '**' param_no_default
fn kwds(input: ParserInput) -> ParseResult<Rc<Expression>> {
    on_error_pass(invalid_kwds)
        .or(right(tok(TT::DOUBLESTAR), param_no_default).map(|p| {
            let s = p.span();
            Rc::new(Expression::Parameters(vec![p.kwargs()], s))
        }))
        .parse(input)
}

// # One parameter.  This *includes* a following comma and type comment.
// #
// # There are three styles:
// # - No default
// # - With default
// # - Maybe with default
// #
// # There are two alternative forms of each, to deal with type comments:
// # - Ends in a comma followed by an optional type comment
// # - No comma, optional type comment, must be followed by close paren
// # The latter form is for a final parameter without trailing comma.
// #

// param_no_default:
//     | param ',' TYPE_COMMENT?
//     | param TYPE_COMMENT? &')'
fn param_no_default(input: ParserInput) -> ParseResult<Parameter> {
    left(param, tok(TT::COMMA))
        .or(left(param, lookahead(tok(TT::RPAR))))
        .parse(input)
}

// param_no_default_star_annotation:
//     | param_star_annotation ',' TYPE_COMMENT?
//     | param_star_annotation TYPE_COMMENT? &')'
fn param_no_default_star_annotation(input: ParserInput) -> ParseResult<Parameter> {
    left(param_star_annotation, tok(TT::COMMA))
        .or(left(param_star_annotation, lookahead(tok(TT::RPAR))))
        .parse(input)
}

// param_with_default:
//     | param default ',' TYPE_COMMENT?
//     | param default TYPE_COMMENT? &')'
fn param_with_default(input: ParserInput) -> ParseResult<Parameter> {
    pair(
        pair(param, default),
        right(tok(TT::COMMA), maybe(tok(TT::TYPE_COMMENT)))
            .or(left(maybe(tok(TT::TYPE_COMMENT)), lookahead(tok(TT::RPAR)))),
    )
    .map(|((mut p, d), t)| {
        p.default = Some(d);
        p.type_comment = t.map(|s| Rc::from(s.lexeme));
        p
    })
    .parse(input)
}

// param_maybe_default:
//     | param default? ',' TYPE_COMMENT?
//     | param default? TYPE_COMMENT? &')'
fn param_maybe_default(input: ParserInput) -> ParseResult<Parameter> {
    pair(
        pair(param, maybe(default)),
        right(tok(TT::COMMA), maybe(tok(TT::TYPE_COMMENT)))
            .or(left(maybe(tok(TT::TYPE_COMMENT)), lookahead(tok(TT::RPAR)))),
    )
    .map(|((mut p, d), t)| {
        p.default = d;
        p.type_comment = t.map(|s| Rc::from(s.lexeme));
        p
    })
    .parse(input)
}

// param: NAME annotation?
fn param(input: ParserInput) -> ParseResult<Parameter> {
    pair(name, maybe(annotation))
        .map(|(n, a)| Parameter::with_annotation(n, a))
        .parse(input)
}

// param_star_annotation: NAME star_annotation
fn param_star_annotation(input: ParserInput) -> ParseResult<Parameter> {
    pair(name, star_annotation)
        .map(|(n, a)| Parameter::with_annotation(n, Some(a)))
        .parse(input)
}

// annotation: ':' expression
fn annotation(input: ParserInput) -> ParseResult<Rc<Expression>> {
    right(tok(TT::COLON), expression).parse(input)
}

// star_annotation: ':' star_expression
fn star_annotation(input: ParserInput) -> ParseResult<Rc<Expression>> {
    right(tok(TT::COLON), star_expression).parse(input)
}

// default: '=' expression  | invalid_default
fn default(input: ParserInput) -> ParseResult<Rc<Expression>> {
    right(tok(TT::EQUAL), expression)
        .or(on_error_pass(invalid_default))
        .parse(input)
}

// # If statement
// # ------------

// if_stmt:
//     | 'if' named_expression ':' block elif_stmt
//     | 'if' named_expression ':' block [else_block]
fn if_stmt(input: ParserInput) -> ParseResult<Statement> {
    on_error_pass(invalid_if_stmt)
        .or(pair(
            pair(
                pair(token_nodiscard(TT::KEYWORD, "if"), named_expression),
                right(tok(TT::COLON), block),
            ),
            elif_stmt.or(maybe(else_block).map(|e| (vec![], e))),
        )
        .map(|(((t, expr), then), (elif, els))| {
            let s = t.span.till_block(&then).or(&els);
            Statement::If(expr, then, elif, els, s)
        }))
        .parse(input)
}

// elif_stmt:
//     | 'elif' named_expression ':' block elif_stmt
//     | 'elif' named_expression ':' block [else_block]
fn elif_stmt(
    input: ParserInput,
) -> ParseResult<(
    Vec<(Rc<Expression>, Vec<Statement>)>,
    Option<Vec<Statement>>,
)> {
    on_error_pass(invalid_elif_stmt)
        .or(pair(
            one_or_more(pair(
                right(token(TT::KEYWORD, "elif"), named_expression),
                right(tok(TT::COLON), block),
            )),
            maybe(else_block),
        ))
        .parse(input)
}

// else_block:
//     | 'else' ':' block
fn else_block(input: ParserInput) -> ParseResult<Vec<Statement>> {
    on_error_pass(invalid_else_stmt)
        .or(right(
            pair(token(TT::KEYWORD, "else"), tok(TT::COLON)),
            block,
        ))
        .parse(input)
}

// # While statement
// # ---------------

// while_stmt:
//     | 'while' named_expression ':' block [else_block]
fn while_stmt(input: ParserInput) -> ParseResult<Statement> {
    on_error_pass(invalid_while_stmt)
        .or(pair(
            pair(
                token_nodiscard(TT::KEYWORD, "while"),
                pair(left(named_expression, tok(TT::COLON)), block),
            ),
            maybe(else_block),
        )
        .map(|((t, (e, b)), els)| {
            let s = t.span.till_block(&b).or(&els);
            Statement::While(Rc::new(e), b, els, s)
        }))
        .parse(input)
}

// # For statement
// # -------------

// for_stmt:
//     | 'for' star_targets 'in' ~ star_expressions ':' [TYPE_COMMENT] block [else_block]
//     | ASYNC 'for' star_targets 'in' ~ star_expressions ':' [TYPE_COMMENT] block [else_block]
fn for_stmt(input: ParserInput) -> ParseResult<Statement> {
    on_error_pass(invalid_for_stmt)
        .or(pair(
            maybe(token(TT::KEYWORD, "async")),
            pair(
                left(
                    pair(token_nodiscard(TT::KEYWORD, "for"), star_targets),
                    token(TT::KEYWORD, "in"),
                ),
                pair(
                    left(star_expressions, tok(TT::COLON)),
                    pair(pair(maybe(tok(TT::TYPE_COMMENT)), block), maybe(else_block)),
                ),
            ),
        )
        .map(|(a, ((t, tgt), (expr, ((tc, blck), els))))| {
            let span = t.span.till_block(&blck).or(&els);
            Statement::For(
                tgt,
                expr,
                blck,
                els,
                tc.map(|s| Rc::new(Expression::TypeComment(Rc::from(s.lexeme), s.span))),
                a.is_some(),
                span,
            )
        }))
        .or(on_error_pass(invalid_for_target))
        .parse(input)
}

// # With statement
// # --------------

// with_stmt:
//     | 'with' '(' ','.with_item+ ','? ')' ':' block
//     | 'with' ','.with_item+ ':' [TYPE_COMMENT] block
//     | ASYNC 'with' '(' ','.with_item+ ','? ')' ':' block
//     | ASYNC 'with' ','.with_item+ ':' [TYPE_COMMENT] block
fn with_stmt(input: ParserInput) -> ParseResult<Statement> {
    on_error_pass(invalid_with_stmt_indent)
        .or(pair(
            maybe(token(TT::KEYWORD, "async")),
            pair(
                pair(
                    left(token_nodiscard(TT::KEYWORD, "with"), tok(TT::LPAR)),
                    left(sep_by(with_item, TT::COMMA), maybe(tok(TT::COMMA))),
                ),
                right(pair(tok(TT::RPAR), tok(TT::COLON)), block),
            ),
        )
        .map(|(a, ((t, w), b))| (a, (t, (w, (None, b)))))
        .or(pair(
            maybe(token(TT::KEYWORD, "async")),
            pair(
                left(token_nodiscard(TT::KEYWORD, "with"), tok(TT::COLON)),
                pair(
                    sep_by(with_item, TT::COMMA),
                    right(tok(TT::COLON), pair(maybe(tok(TT::TYPE_COMMENT)), block)),
                ),
            ),
        ))
        .map(|(a, (wt, (w, (t, b))))| {
            let s = wt.span.till_block(&b);
            Statement::With(
                w,
                b,
                t.map(|s| Rc::new(Expression::TypeComment(Rc::from(s.lexeme), s.span))),
                a.is_some(),
                s,
            )
        }))
        .or(on_error_pass(invalid_with_stmt))
        .parse(input)
}

// with_item:
//     | expression ['as' star_target &(',' | ')' | ':')]
fn with_item(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        expression,
        maybe(left(
            right(token(TT::KEYWORD, "as"), star_target),
            lookahead(tok(TT::COMMA).or(tok(TT::RPAR)).or(tok(TT::COLON))),
        )),
    )
    .map(|(e, t)| {
        let s = e.span().till(&e).or(&t);
        Rc::new(Expression::WithItem(e, t, s))
    })
    .or(on_error_pass(invalid_with_item))
    .parse(input)
}

// # Try statement
// # -------------

// try_stmt:
//     | 'try' ':' block finally_block
//     | 'try' ':' block except_block+ [else_block] [finally_block]
//     | 'try' ':' block except_star_block+ [else_block] [finally_block]
fn try_stmt(input: ParserInput) -> ParseResult<Statement> {
    on_error_pass(invalid_try_stmt)
        .or(pair(
            left(token_nodiscard(TT::KEYWORD, "try"), tok(TT::COLON)),
            pair(block, finally_block),
        )
        .map(|(t, (b, f))| {
            let s = t.span.till_block(&f);
            Statement::Try(b, vec![], None, Some(f), s)
        }))
        .or(pair(
            left(token_nodiscard(TT::KEYWORD, "try"), tok(TT::COLON)),
            pair(
                pair(block, one_or_more(except_block)),
                pair(maybe(else_block), maybe(finally_block)),
            ),
        )
        .map(|(t, ((b, ex), (e, f)))| {
            let s = t.span.till(&ex).or(&e).or(&f);
            Statement::Try(b, ex, e, f, s)
        }))
        .or(pair(
            left(token_nodiscard(TT::KEYWORD, "try"), tok(TT::COLON)),
            pair(
                pair(block, one_or_more(except_star_block)),
                pair(maybe(else_block), maybe(finally_block)),
            ),
        )
        .map(|(t, ((b, ex), (e, f)))| {
            let s = t.span.till_block(&b).or(&e).or(&f);
            Statement::Try(b, ex, e, f, s)
        }))
        .parse(input)
}

// # Except statement
// # ----------------

// except_block:
//     | 'except' expression ['as' NAME ] ':' block
//     | 'except' ':' block
fn except_block(input: ParserInput) -> ParseResult<Rc<Expression>> {
    on_error_pass(invalid_except_stmt_indent)
        .or(pair(
            pair(
                token_nodiscard(TT::KEYWORD, "except"),
                pair(expression, maybe(right(token(TT::KEYWORD, "as"), name))),
            ),
            block,
        )
        .map(|((t, (e, a)), b)| {
            let s = t.span.till_block(&b);
            Rc::new(Expression::ExceptBlock(Some(e), a, b, false, s))
        }))
        .or(pair(
            left(token_nodiscard(TT::KEYWORD, "except"), tok(TT::COLON)),
            block,
        )
        .map(|(t, b)| {
            let s = t.span.till_block(&b);
            Rc::new(Expression::ExceptBlock(None, None, b, false, s))
        }))
        .or(on_error_pass(invalid_except_stmt))
        .parse(input)
}

// except_star_block:
//     | 'except' '*' expression ['as' NAME ] ':' block
fn except_star_block(input: ParserInput) -> ParseResult<Rc<Expression>> {
    on_error_pass(invalid_except_star_stmt_indent)
        .or(pair(
            pair(
                left(token_nodiscard(TT::KEYWORD, "except"), tok(TT::STAR)),
                pair(expression, maybe(right(token(TT::KEYWORD, "as"), name))),
            ),
            block,
        )
        .map(|((t, (e, a)), b)| {
            let s = t.span.till_block(&b);
            Rc::new(Expression::ExceptBlock(Some(e), a, b, true, s))
        }))
        .or(on_error_pass(invalid_except_stmt))
        .parse(input)
}

// finally_block:
//     | 'finally' ':' block
fn finally_block(input: ParserInput) -> ParseResult<Vec<Statement>> {
    on_error_pass(invalid_finally_stmt)
        .or(right(
            pair(token(TT::KEYWORD, "finally"), tok(TT::COLON)),
            block,
        ))
        .parse(input)
}

// # Match statement
// # ---------------

// match_stmt:
//     | "match" subject_expr ':' NEWLINE INDENT case_block+ DEDENT
fn match_stmt(input: ParserInput) -> ParseResult<Statement> {
    pair(
        pair(token_nodiscard(TT::NAME, "match"), subject_expr),
        right(
            pair(pair(tok(TT::COLON), tok(TT::NEWLINE)), tok(TT::INDENT)),
            left(one_or_more(case_block), tok(TT::DEDENT)),
        ),
    )
    .map(|((t, s), cs)| {
        let sp = t.span.till(&cs);
        Statement::Match(s, cs, sp)
    })
    .or(on_error_pass(invalid_match_stmt))
    .parse(input)
}

// subject_expr:
//     | star_named_expression ',' star_named_expressions?
//     | named_expression
fn subject_expr(input: ParserInput) -> ParseResult<Vec<Rc<Expression>>> {
    pair(
        left(star_named_expression, tok(TT::COMMA)),
        maybe(star_named_expressions),
    )
    .map(|(e, es)| {
        let mut exprs = vec![e];
        exprs.extend(es.unwrap_or_default());
        exprs
    })
    .or(named_expression.map(|e| vec![e]))
    .parse(input)
}

// case_block:
//     | "case" patterns guard? ':' block
fn case_block(input: ParserInput) -> ParseResult<Rc<Expression>> {
    on_error_pass(invalid_case_block)
        .or(pair(
            pair(token_nodiscard(TT::NAME, "case"), patterns),
            pair(left(maybe(guard), tok(TT::COLON)), block),
        )
        .map(|((t, p), (g, b))| {
            let s = t.span.till_block(&b);
            Rc::new(Expression::Case(p, g, b, s))
        }))
        .parse(input)
}

// guard: 'if' named_expression
fn guard(input: ParserInput) -> ParseResult<Rc<Expression>> {
    right(token(TT::KEYWORD, "if"), named_expression).parse(input)
}

// patterns:
//     | open_sequence_pattern
//     | pattern
fn patterns(input: ParserInput) -> ParseResult<Vec<Pattern>> {
    open_sequence_pattern
        .or(pattern.map(|p| vec![p]))
        .parse(input)
}

// pattern:
//     | as_pattern
//     | or_pattern
fn pattern(input: ParserInput) -> ParseResult<Pattern> {
    as_pattern.or(or_pattern).parse(input)
}

// as_pattern:
//     | or_pattern 'as' pattern_capture_target
fn as_pattern(input: ParserInput) -> ParseResult<Pattern> {
    pair(
        left(or_pattern, token(TT::KEYWORD, "as")),
        pattern_capture_target,
    )
    .map(|(p, t)| Pattern::Capture(Some(Rc::new(p)), t))
    .or(on_error_pass(invalid_as_pattern))
    .parse(input)
}

// or_pattern:
//     | '|'.closed_pattern+
fn or_pattern(input: ParserInput) -> ParseResult<Pattern> {
    sep_by(closed_pattern, TT::VBAR)
        .map(Pattern::Disjunction)
        .parse(input)
}

// closed_pattern:
//     | literal_pattern
//     | capture_pattern
//     | wildcard_pattern
//     | value_pattern
//     | group_pattern
//     | sequence_pattern
//     | mapping_pattern
//     | class_pattern
fn closed_pattern(input: ParserInput) -> ParseResult<Pattern> {
    literal_pattern
        .or(capture_pattern)
        .or(wildcard_pattern)
        .or(value_pattern)
        .or(group_pattern)
        .or(sequence_pattern)
        .or(mapping_pattern)
        .or(class_pattern)
        .parse(input)
}

// # Literal patterns are used for equality and identity constraints
// literal_pattern:
//     | signed_number !('+' | '-')
//     | complex_number
//     | strings
//     | 'None'
//     | 'True'
//     | 'False'
fn literal_pattern(input: ParserInput) -> ParseResult<Pattern> {
    number
        .or(strings)
        .or(token_nodiscard(TT::KEYWORD, "None").map(|t| Rc::new(Expression::None(t.span))))
        .or(token_nodiscard(TT::KEYWORD, "True").map(|t| Rc::new(Expression::True(t.span))))
        .or(token_nodiscard(TT::KEYWORD, "False").map(|t| Rc::new(Expression::False(t.span))))
        .map(Pattern::Literal)
        .parse(input)
}

// # Literal expressions are used to restrict permitted mapping pattern keys
// literal_expr:
//     | signed_number !('+' | '-')
//     | complex_number
//     | strings
//     | 'None'
//     | 'True'
//     | 'False'
fn literal_expr(input: ParserInput) -> ParseResult<Expression> {
    literal_pattern
        .map(|p| {
            let s = p.span();
            Expression::Pattern(Rc::new(p), s)
        })
        .parse(input)
}

// capture_pattern:
//     | pattern_capture_target
fn capture_pattern(input: ParserInput) -> ParseResult<Pattern> {
    pattern_capture_target
        .map(|n| Pattern::Capture(None, n))
        .parse(input)
}

// pattern_capture_target:
//     | !"_" NAME !('.' | '(' | '=')
fn pattern_capture_target(input: ParserInput) -> ParseResult<Name> {
    left(
        right(not(wildcard_pattern), name),
        not(tok(TT::DOT).or(tok(TT::LPAR)).or(tok(TT::EQUAL))),
    )
    .parse(input)
}

// wildcard_pattern:
//     | "_"
fn wildcard_pattern(input: ParserInput) -> ParseResult<Pattern> {
    tok(TT::NAME)
        .pred(|t| t.lexeme.as_str() == "_")
        .map(|_| Pattern::Wildcard)
        .parse(input)
}

// value_pattern:
//     | attr !('.' | '(' | '=')
fn value_pattern(input: ParserInput) -> ParseResult<Pattern> {
    left(attr, not(tok(TT::DOT).or(tok(TT::LPAR)).or(tok(TT::EQUAL))))
        .map(|a| match a {
            Expression::Attribute(ns, _) => Pattern::Value(ns),
            _ => unreachable!(),
        })
        .parse(input)
}

// attr:
//     | (attr | NAME) '.' NAME
fn attr(input: ParserInput) -> ParseResult<Expression> {
    pair(left(name, tok(TT::DOT)), sep_by(name, TT::DOT))
        .map(|(n, mut ns)| {
            ns.insert(0, n);
            let s = ns.span();
            Expression::Attribute(ns, s)
        })
        .parse(input)
}

fn name_or_attr(input: ParserInput) -> ParseResult<Expression> {
    name.map(|n| {
        let s = n.span();
        Expression::Attribute(vec![n], s)
    })
    .or(attr)
    .parse(input)
}

// group_pattern:
//     | '(' pattern ')'
fn group_pattern(input: ParserInput) -> ParseResult<Pattern> {
    right(tok(TT::LPAR), left(pattern, tok(TT::RPAR)))
        .map(|p| Pattern::Group(Rc::new(p)))
        .parse(input)
}

// sequence_pattern:
//     | '[' maybe_sequence_pattern? ']'
//     | '(' open_sequence_pattern? ')'
fn sequence_pattern(input: ParserInput) -> ParseResult<Pattern> {
    right(
        tok(TT::LSQB),
        left(maybe(maybe_sequence_pattern), tok(TT::RSQB)),
    )
    .or(right(
        tok(TT::LPAR),
        left(maybe(open_sequence_pattern), tok(TT::RPAR)),
    ))
    .map(|s| Pattern::Sequence(s.unwrap_or_default()))
    .parse(input)
}

// open_sequence_pattern:
//     | maybe_star_pattern ',' maybe_sequence_pattern?
fn open_sequence_pattern(input: ParserInput) -> ParseResult<Vec<Pattern>> {
    pair(
        left(maybe_star_pattern, tok(TT::COMMA)),
        maybe(maybe_sequence_pattern),
    )
    .map(|(u, v)| {
        let mut seq: Vec<Pattern> = Vec::new();
        seq.push(u);
        seq.extend(v.unwrap_or_default());
        seq
    })
    .parse(input)
}

// maybe_sequence_pattern:
//     | ','.maybe_star_pattern+ ','?
fn maybe_sequence_pattern(input: ParserInput) -> ParseResult<Vec<Pattern>> {
    left(sep_by(maybe_star_pattern, TT::COMMA), maybe(tok(TT::COMMA))).parse(input)
}

// maybe_star_pattern:
//     | star_pattern
//     | pattern
fn maybe_star_pattern(input: ParserInput) -> ParseResult<Pattern> {
    star_pattern.or(pattern).parse(input)
}

// star_pattern:
//     | '*' pattern_capture_target
//     | '*' wildcard_pattern
fn star_pattern(input: ParserInput) -> ParseResult<Pattern> {
    right(
        tok(TT::STAR),
        pattern_capture_target
            .map(|n| Pattern::Capture(None, n))
            .or(wildcard_pattern),
    )
    .map(|p| Pattern::Star(Rc::new(p)))
    .parse(input)
}

// mapping_pattern:
//     | '{' '}'
//     | '{' double_star_pattern ','? '}'
//     | '{' items_pattern ',' double_star_pattern ','? '}'
//     | '{' items_pattern ','? '}'
fn mapping_pattern(input: ParserInput) -> ParseResult<Pattern> {
    right(
        tok(TT::LBRACE),
        tok(TT::RBRACE)
            .map(|_| vec![])
            .or(left(
                double_star_pattern,
                pair(maybe(tok(TT::COMMA)), tok(TT::RBRACE)),
            )
            .map(|p| vec![p]))
            .or(left(
                pair(left(items_pattern, tok(TT::COMMA)), double_star_pattern),
                pair(maybe(tok(TT::COMMA)), tok(TT::RBRACE)),
            )
            .map(|(mut is, p)| {
                is.push(p);
                is
            }))
            .or(left(
                items_pattern,
                pair(maybe(tok(TT::COMMA)), tok(TT::RBRACE)),
            )),
    )
    .map(Pattern::Mapping)
    .parse(input)
}

// items_pattern:
//     | ','.key_value_pattern+
fn items_pattern(input: ParserInput) -> ParseResult<Vec<Pattern>> {
    sep_by(key_value_pattern, TT::COMMA).parse(input)
}

// key_value_pattern:
//     | (literal_expr | attr) ':' pattern
fn key_value_pattern(input: ParserInput) -> ParseResult<Pattern> {
    pair(left(literal_expr.or(attr), tok(TT::COLON)), pattern)
        .map(|(e, p)| Pattern::KeyValue(Rc::new(e), Rc::new(p)))
        .parse(input)
}

// double_star_pattern:
//     | '**' pattern_capture_target
fn double_star_pattern(input: ParserInput) -> ParseResult<Pattern> {
    right(tok(TT::DOUBLESTAR), pattern_capture_target)
        .map(Pattern::DoubleStar)
        .parse(input)
}

// class_pattern:
//     | name_or_attr '(' ')'
//     | name_or_attr '(' positional_patterns ','? ')'
//     | name_or_attr '(' keyword_patterns ','? ')'
//     | name_or_attr '(' positional_patterns ',' keyword_patterns ','? ')'
fn class_pattern(input: ParserInput) -> ParseResult<Pattern> {
    pair(
        name_or_attr,
        pair(tok(TT::LPAR), tok(TT::RPAR))
            .map(|_| vec![])
            .or(right(
                tok(TT::LPAR),
                left(
                    positional_patterns,
                    pair(maybe(tok(TT::COMMA)), tok(TT::RPAR)),
                ),
            ))
            .or(right(
                tok(TT::LPAR),
                left(keyword_patterns, pair(maybe(tok(TT::COMMA)), tok(TT::RPAR))),
            ))
            .or(right(
                tok(TT::LPAR),
                pair(
                    positional_patterns,
                    right(tok(TT::COMMA), left(keyword_patterns, tok(TT::RPAR))),
                ),
            )
            .map(|(mut pos, kwd)| {
                pos.extend(kwd);
                pos
            })),
    )
    .map(|(n, ps)| Pattern::Class(Rc::new(n), ps))
    .or(on_error_pass(invalid_class_pattern))
    .parse(input)
}

// positional_patterns:
//     | ','.pattern+
fn positional_patterns(input: ParserInput) -> ParseResult<Vec<Pattern>> {
    sep_by(pattern, TT::COMMA).parse(input)
}

// keyword_patterns:
//     | ','.keyword_pattern+
fn keyword_patterns(input: ParserInput) -> ParseResult<Vec<Pattern>> {
    sep_by(keyword_pattern, TT::COMMA).parse(input)
}

// keyword_pattern:
//     | NAME '=' pattern
fn keyword_pattern(input: ParserInput) -> ParseResult<Pattern> {
    pair(left(name, tok(TT::EQUAL)), pattern)
        .map(|(n, p)| {
            let s = n.span.till(&p);
            Pattern::KeyValue(Rc::new(Expression::Name(n, s)), Rc::new(p))
        })
        .parse(input)
}

// # Type statement
// # ---------------

// type_alias:
//     | "type" NAME [type_params] '=' expression

fn type_alias(input: ParserInput) -> ParseResult<Statement> {
    pair(
        pair(token_nodiscard(TT::NAME, "type"), name),
        pair(left(maybe(type_params), tok(TT::EQUAL)), expression),
    )
    .map(|((tt, n), (t, e))| {
        let s = tt.span.till(&e);
        Statement::Type(n, t.unwrap_or_default(), e, s)
    })
    .parse(input)
}

// # Type parameter declaration
// # --------------------------

// type_params: '[' type_param_seq  ']'
fn type_params(input: ParserInput) -> ParseResult<Vec<Rc<Expression>>> {
    left(right(tok(TT::LSQB), type_param_seq), tok(TT::RSQB)).parse(input)
}

// type_param_seq: ','.type_param+ [',']
fn type_param_seq(input: ParserInput) -> ParseResult<Vec<Rc<Expression>>> {
    left(sep_by(type_param, TT::COMMA), maybe(tok(TT::COMMA))).parse(input)
}

// type_param:
//     | NAME [type_param_bound]
//     | '*' NAME ':' expression
//     | '*' NAME
//     | '**' NAME ':' expression
//     | '**' NAME
fn type_param(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(name, maybe(type_param_bound))
        .map(|(name, type_bound)| {
            let span = name.span.or(&type_bound);
            Rc::new(Expression::TypeBound(
                TypeBound {
                    name,
                    type_bound,
                    starred: false,
                    double_starred: false,
                },
                span,
            ))
        })
        .or(pair(
            right(tok(TT::STAR), name),
            right(tok(TT::COLON), expression),
        )
        .map(|(name, t)| {
            let span = name.span.till(&t);
            Rc::new(Expression::TypeBound(
                TypeBound {
                    name,
                    type_bound: Some(t),
                    starred: true,
                    double_starred: false,
                },
                span,
            ))
        }))
        .or(right(tok(TT::STAR), name).map(|name| {
            let span = name.span;
            Rc::new(Expression::TypeBound(
                TypeBound {
                    name,
                    type_bound: None,
                    starred: true,
                    double_starred: false,
                },
                span,
            ))
        }))
        .or(pair(
            right(tok(TT::DOUBLESTAR), name),
            right(tok(TT::COLON), expression),
        )
        .map(|(name, t)| {
            let span = name.span.till(&t);
            Rc::new(Expression::TypeBound(
                TypeBound {
                    name,
                    type_bound: Some(t),
                    starred: false,
                    double_starred: true,
                },
                span,
            ))
        }))
        .or(right(tok(TT::DOUBLESTAR), name).map(|name| {
            let span = name.span;
            Rc::new(Expression::TypeBound(
                TypeBound {
                    name,
                    type_bound: None,
                    starred: false,
                    double_starred: true,
                },
                span,
            ))
        }))
        .parse(input)
}

// type_param_bound: ':' expression
fn type_param_bound(input: ParserInput) -> ParseResult<Rc<Expression>> {
    right(tok(TT::COLON), expression).parse(input)
}

// # EXPRESSIONS
// # -----------

// expressions:
//     | expression (',' expression )+ [',']
//     | expression ','
//     | expression
fn expressions(input: ParserInput) -> ParseResult<Vec<Rc<Expression>>> {
    left(sep_by(expression, TT::COMMA), maybe(tok(TT::COMMA))).parse(input)
}

// expression:
//     | disjunction 'if' disjunction 'else' expression
//     | disjunction
//     | lambdef
fn expression(input: ParserInput) -> ParseResult<Rc<Expression>> {
    if let Some(result) = try_remember(input, expression) {
        return result;
    }
    let result = on_error_pass(invalid_expression.or(invalid_legacy_expression))
        .or(pair(
            disjunction,
            maybe(pair(
                right(token(TT::KEYWORD, "if"), disjunction),
                right(token(TT::KEYWORD, "else"), expression),
            )),
        )
        .map(|(t, o)| match o {
            Some((c, e)) => {
                let s = t.span().till(&e);
                Rc::new(Expression::Ternary(c, t, e, s))
            }
            None => t,
        }))
        .or(lambdef)
        .parse(input);
    save_result(input, expression, &result);
    result
}

// yield_expr:
//     | 'yield' 'from' expression
//     | 'yield' [star_expressions]
fn yield_expr(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        left(
            token_nodiscard(TT::KEYWORD, "yield"),
            token(TT::KEYWORD, "from"),
        ),
        expression,
    )
    .map(|(t, e)| {
        let s = t.span.till(&e);
        Rc::new(Expression::YieldFrom(e, s))
    })
    .or(pair(
        token_nodiscard(TT::KEYWORD, "yield"),
        maybe(star_expressions).map(|v| v.unwrap_or_default()),
    )
    .map(|(t, e)| {
        let s = t.span.till(&e);
        Rc::new(Expression::Yield(e, s))
    }))
    .parse(input)
}

// star_expressions:
//     | star_expression (',' star_expression )+ [',']
//     | star_expression ','
//     | star_expression
fn star_expressions(input: ParserInput) -> ParseResult<Vec<Rc<Expression>>> {
    left(sep_by(star_expression, TT::COMMA), maybe(tok(TT::COMMA))).parse(input)
}

// star_expression:
//     | '*' bitwise_or
//     | expression
fn star_expression(input: ParserInput) -> ParseResult<Rc<Expression>> {
    if let Some(result) = try_remember(input, star_expression) {
        return result;
    }
    let result = pair(tok(TT::STAR), bitwise_or)
        .map(|(t, e)| {
            let s = t.span.till(&e);
            Rc::new(Expression::ListUnwrap(e, s))
        })
        .or(expression)
        .parse(input);
    save_result(input, star_expression, &result);
    result
}

// star_named_expressions: ','.star_named_expression+ [',']
fn star_named_expressions(input: ParserInput) -> ParseResult<Vec<Rc<Expression>>> {
    left(
        sep_by(star_named_expression, TT::COMMA),
        maybe(tok(TT::COMMA)),
    )
    .parse(input)
}

// star_named_expression:
//     | '*' bitwise_or
//     | named_expression
fn star_named_expression(input: ParserInput) -> ParseResult<Rc<Expression>> {
    right(tok(TT::STAR), bitwise_or)
        .or(named_expression)
        .parse(input)
}

// assignment_expression:
//     | NAME ':=' ~ expression
fn assignment_expression(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        left(
            name.map(|n| {
                let s = n.span;
                Rc::new(Expression::Name(n, s))
            }),
            tok(TT::COLONEQUAL),
        ),
        expression,
    )
    .map(|(l, r)| {
        let s = l.span().till(&r);
        Rc::new(Expression::Walrus(l, r, s))
    })
    .parse(input)
}

// named_expression:
//     | assignment_expression
//     | expression !':='
fn named_expression(input: ParserInput) -> ParseResult<Rc<Expression>> {
    assignment_expression
        .or(on_error_pass(invalid_named_expression))
        .or(left(expression, not(tok(TT::COLONEQUAL))))
        .parse(input)
}

// disjunction:
//     | conjunction ('or' conjunction )+
//     | conjunction
fn disjunction(input: ParserInput) -> ParseResult<Rc<Expression>> {
    if let Some(result) = try_remember(input, disjunction) {
        return result;
    }
    let result = pair(conjunction, right(token(TT::KEYWORD, "or"), conjunction))
        .map(|(l, r)| {
            let s = l.span().till(&r);
            Rc::new(Expression::BinaryOperation(Operator::Or, l, r, s))
        })
        .or(conjunction)
        .parse(input);
    save_result(input, disjunction, &result);
    result
}

// conjunction:
//     | inversion ('and' inversion )+
//     | inversion
fn conjunction(input: ParserInput) -> ParseResult<Rc<Expression>> {
    if let Some(result) = try_remember(input, conjunction) {
        return result;
    }
    let result = pair(inversion, right(token(TT::KEYWORD, "and"), inversion))
        .map(|(l, r)| {
            let s = l.span().till(&r);
            Rc::new(Expression::BinaryOperation(Operator::And, l, r, s))
        })
        .or(inversion)
        .parse(input);
    save_result(input, conjunction, &result);
    result
}

// inversion:
//     | 'not' inversion
//     | comparison
fn inversion(input: ParserInput) -> ParseResult<Rc<Expression>> {
    if let Some(result) = try_remember(input, inversion) {
        return result;
    }
    let result = pair(token_nodiscard(TT::KEYWORD, "not"), inversion)
        .map(|(t, i)| {
            let s = t.span.till(&i);
            Rc::new(Expression::UnaryOperation(Operator::Not, i, s))
        })
        .or(comparison)
        .parse(input);
    save_result(input, inversion, &result);
    result
}

// # Comparison operators
// # --------------------

// comparison:
//     | bitwise_or compare_op_bitwise_or_pair+
//     | bitwise_or
fn comparison(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(bitwise_or, zero_or_more(compare_op_bitwise_or_pair))
        .map(|(l, r)| {
            if r.is_empty() {
                l
            } else {
                let end = &r.last().unwrap().1;
                let s = l.span().till(end);
                Rc::new(Expression::Comparison(l, r, s))
            }
        })
        .parse(input)
}

// compare_op_bitwise_or_pair:
//     | eq_bitwise_or
//     | noteq_bitwise_or
//     | lte_bitwise_or
//     | lt_bitwise_or
//     | gte_bitwise_or
//     | gt_bitwise_or
//     | notin_bitwise_or
//     | in_bitwise_or
//     | isnot_bitwise_or
//     | is_bitwise_or
fn compare_op_bitwise_or_pair(input: ParserInput) -> ParseResult<(Operator, Rc<Expression>)> {
    eq_bitwise_or
        .or(noteq_bitwise_or)
        .or(lte_bitwise_or)
        .or(lt_bitwise_or)
        .or(gte_bitwise_or)
        .or(gt_bitwise_or)
        .or(notin_bitwise_or)
        .or(in_bitwise_or)
        .or(isnot_bitwise_or)
        .or(is_bitwise_or)
        .parse(input)
}

// eq_bitwise_or: '==' bitwise_or
fn eq_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Rc<Expression>)> {
    pair(tok(TT::EQEQUAL).map(Operator::from), bitwise_or).parse(input)
}

// noteq_bitwise_or:
//     | ('!=' ) bitwise_or
fn noteq_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Rc<Expression>)> {
    pair(tok(TT::NOTEQUAL).map(Operator::from), bitwise_or).parse(input)
}

// lte_bitwise_or: '<=' bitwise_or
fn lte_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Rc<Expression>)> {
    pair(tok(TT::LESSEQUAL).map(Operator::from), bitwise_or).parse(input)
}

// lt_bitwise_or: '<' bitwise_or
fn lt_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Rc<Expression>)> {
    pair(tok(TT::LESS).map(Operator::from), bitwise_or).parse(input)
}

// gte_bitwise_or: '>=' bitwise_or
fn gte_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Rc<Expression>)> {
    pair(tok(TT::GREATEREQUAL).map(Operator::from), bitwise_or).parse(input)
}

// gt_bitwise_or: '>' bitwise_or
fn gt_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Rc<Expression>)> {
    pair(tok(TT::GREATER).map(Operator::from), bitwise_or).parse(input)
}

// notin_bitwise_or: 'not' 'in' bitwise_or
fn notin_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Rc<Expression>)> {
    right(
        pair(token(TT::KEYWORD, "not"), token(TT::KEYWORD, "in")),
        bitwise_or,
    )
    .map(|e| (Operator::NotIn, e))
    .parse(input)
}

// in_bitwise_or: 'in' bitwise_or
fn in_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Rc<Expression>)> {
    right(token(TT::KEYWORD, "in"), bitwise_or)
        .map(|e| (Operator::In, e))
        .parse(input)
}

// isnot_bitwise_or: 'is' 'not' bitwise_or
fn isnot_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Rc<Expression>)> {
    right(
        pair(token(TT::KEYWORD, "is"), token(TT::KEYWORD, "not")),
        bitwise_or,
    )
    .map(|e| (Operator::IsNot, e))
    .parse(input)
}

// is_bitwise_or: 'is' bitwise_or
fn is_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Rc<Expression>)> {
    right(token(TT::KEYWORD, "is"), bitwise_or)
        .map(|e| (Operator::Is, e))
        .parse(input)
}

// # Bitwise operators
// # -----------------

// bitwise_or_tail:
//     | '|' bitwise_xor bitwise_or_tail
//     | epsilon
fn bitwise_or_tail(input: ParserInput) -> ParseResult<IncompleteExpression> {
    right(tok(TT::VBAR), pair(bitwise_xor, bitwise_or_tail))
        .map(|(e, t)| {
            IncompleteExpression::BinaryOperation(Operator::BitwiseOr, Box::new(e), Box::new(t))
        })
        .or(epsilon.map(|_| IncompleteExpression::Empty))
        .parse(input)
}

// bitwise_or:
//     | bitwise_xor bitwise_or_tail
fn bitwise_or(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(bitwise_xor, bitwise_or_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::BinaryOperation(_, expr, tail) => {
                        let s = expr.span().till(&current_expr);
                        (
                            Rc::new(Expression::BinaryOperation(
                                Operator::BitwiseOr,
                                current_expr,
                                *expr,
                                s,
                            )),
                            tail,
                        )
                    }
                    IncompleteExpression::Invalid => (
                        Rc::new(Expression::Invalid(current_expr.span())),
                        Box::new(IncompleteExpression::Empty),
                    ),
                    _ => unreachable!(),
                };
            }
            current_expr
        })
        .parse(input)
}

// bitwise_xor_tail:
//     | '^' bitwise_and bitwise_xor_tail
//     | epsilon
fn bitwise_xor_tail(input: ParserInput) -> ParseResult<IncompleteExpression> {
    right(tok(TT::CIRCUMFLEX), pair(bitwise_and, bitwise_xor_tail))
        .map(|(e, t)| {
            IncompleteExpression::BinaryOperation(Operator::BitwiseXor, Box::new(e), Box::new(t))
        })
        .or(epsilon.map(|_| IncompleteExpression::Empty))
        .parse(input)
}

// bitwise_xor:
//     | bitwise_and bitwise_xor_tail
fn bitwise_xor(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(bitwise_and, bitwise_xor_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::BinaryOperation(_, expr, tail) => {
                        let s = expr.span().till(&current_expr);
                        (
                            Rc::new(Expression::BinaryOperation(
                                Operator::BitwiseXor,
                                current_expr,
                                *expr,
                                s,
                            )),
                            tail,
                        )
                    }
                    IncompleteExpression::Invalid => (
                        Rc::new(Expression::Invalid(current_expr.span())),
                        Box::new(IncompleteExpression::Empty),
                    ),
                    _ => unreachable!(),
                };
            }
            current_expr
        })
        .parse(input)
}

// bitwise_and_tail:
//     | '&' shift_expr bitwise_and_tail
//     | epsilon
fn bitwise_and_tail(input: ParserInput) -> ParseResult<IncompleteExpression> {
    right(tok(TT::VBAR), pair(shift_expr, bitwise_and_tail))
        .map(|(e, t)| {
            IncompleteExpression::BinaryOperation(Operator::BitwiseAnd, Box::new(e), Box::new(t))
        })
        .or(epsilon.map(|_| IncompleteExpression::Empty))
        .parse(input)
}

// bitwise_and:
//     | shift_expr bitwise_and_tail
fn bitwise_and(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(shift_expr, bitwise_and_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::BinaryOperation(_, expr, tail) => {
                        let s = expr.span().till(&current_expr);
                        (
                            Rc::new(Expression::BinaryOperation(
                                Operator::BitwiseAnd,
                                current_expr,
                                *expr,
                                s,
                            )),
                            tail,
                        )
                    }
                    IncompleteExpression::Invalid => (
                        Rc::new(Expression::Invalid(current_expr.span())),
                        Box::new(IncompleteExpression::Empty),
                    ),
                    _ => unreachable!(),
                };
            }
            current_expr
        })
        .parse(input)
}

// shift_expr_tail:
//     | '<<' sum shiftexpr_tail
//     | '>>' sum shiftexpr_tail
//     | epsilon
fn shift_expr_tail(input: ParserInput) -> ParseResult<IncompleteExpression> {
    pair(
        tok(TT::LEFTSHIFT).or(tok(TT::RIGHTSHIFT)),
        pair(sum, shift_expr_tail),
    )
    .map(|(o, (e, t))| IncompleteExpression::BinaryOperation(o.into(), Box::new(e), Box::new(t)))
    .or(epsilon.map(|_| IncompleteExpression::Empty))
    .parse(input)
}

// shift_expr:
//     | sum shift_expr_tail
fn shift_expr(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(sum, shift_expr_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::BinaryOperation(op, expr, tail) => {
                        let s = expr.span().till(&current_expr);
                        (
                            Rc::new(Expression::BinaryOperation(op, current_expr, *expr, s)),
                            tail,
                        )
                    }
                    IncompleteExpression::Invalid => (
                        Rc::new(Expression::Invalid(current_expr.span())),
                        Box::new(IncompleteExpression::Empty),
                    ),
                    _ => unreachable!(),
                };
            }
            current_expr
        })
        .parse(input)
}

// # Arithmetic operators
// # --------------------

// sum_tail:
//     | '+' term sum_tail
//     | '-' term sum_tail
//     | epsilon
fn sum_tail(input: ParserInput) -> ParseResult<IncompleteExpression> {
    pair(tok(TT::PLUS).or(tok(TT::MINUS)), pair(term, sum_tail))
        .map(|(o, (e, t))| {
            IncompleteExpression::BinaryOperation(o.into(), Box::new(e), Box::new(t))
        })
        .or(epsilon.map(|_| IncompleteExpression::Empty))
        .parse(input)
}

// sum:
//     | term sum_tail
fn sum(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(term, sum_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::BinaryOperation(op, expr, tail) => {
                        let s = expr.span().till(&current_expr);
                        (
                            Rc::new(Expression::BinaryOperation(op, current_expr, *expr, s)),
                            tail,
                        )
                    }
                    IncompleteExpression::Invalid => (
                        Rc::new(Expression::Invalid(current_expr.span())),
                        Box::new(IncompleteExpression::Empty),
                    ),
                    _ => unreachable!(),
                };
            }
            current_expr
        })
        .parse(input)
}

// term_tail:
//     | '*' factor term_tail
//     | '/' factor term_tail
//     | '//' factor term_tail
//     | '%' factor term_tail
//     | '@' factor term_tail
//     | epsilon
fn term_tail(input: ParserInput) -> ParseResult<IncompleteExpression> {
    pair(
        tok(TT::STAR)
            .or(tok(TT::SLASH))
            .or(tok(TT::DOUBLESLASH))
            .or(tok(TT::PERCENT))
            .or(tok(TT::AT)),
        pair(factor, term_tail),
    )
    .map(|(o, (e, t))| IncompleteExpression::BinaryOperation(o.into(), Box::new(e), Box::new(t)))
    .or(epsilon.map(|_| IncompleteExpression::Empty))
    .parse(input)
}

// term:
//     | factor term_tail
fn term(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(factor, term_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::BinaryOperation(op, expr, tail) => {
                        let s = expr.span().till(&current_expr);
                        (
                            Rc::new(Expression::BinaryOperation(op, current_expr, *expr, s)),
                            tail,
                        )
                    }
                    IncompleteExpression::Invalid => (
                        Rc::new(Expression::Invalid(current_expr.span())),
                        Box::new(IncompleteExpression::Empty),
                    ),
                    _ => unreachable!(),
                };
            }
            current_expr
        })
        .parse(input)
}

// factor:
//     | '+' factor
//     | '-' factor
//     | '~' factor
//     | power
fn factor(input: ParserInput) -> ParseResult<Rc<Expression>> {
    if let Some(result) = try_remember(input, factor) {
        return result;
    }
    let result = power
        .or(
            pair(tok(TT::PLUS).or(tok(TT::MINUS)).or(tok(TT::TILDE)), factor).map(|(ref o, e)| {
                let op = o.into();
                match op {
                    Operator::Minus | Operator::BitwiseNot => {
                        let s = o.span.till(&e);
                        Rc::new(Expression::UnaryOperation(op, e, s))
                    }
                    _ => e,
                }
            }),
        )
        .parse(input);
    save_result(input, factor, &result);
    result
}

// power:
//     | await_primary ['**' factor]
fn power(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(await_primary, maybe(pair(tok(TT::DOUBLESTAR), factor)))
        .map(|(l, exp)| match exp {
            Some((o, r)) => {
                let s = l.span().till(&r);
                Rc::new(Expression::BinaryOperation(o.into(), l, r, s))
            }
            None => l,
        })
        .parse(input)
}

// # Primary elements
// # ----------------

// # Primary elements are things like "obj.something.something", "obj[something]", "obj(something)", "obj" ...

// await_primary:
//     | AWAIT primary
//     | primary
fn await_primary(input: ParserInput) -> ParseResult<Rc<Expression>> {
    if let Some(result) = try_remember(input, await_primary) {
        return result;
    }
    let result = right(token(TT::KEYWORD, "await"), primary)
        .or(primary)
        .parse(input);
    save_result(input, await_primary, &result);
    result
}

// primary:
//   | atom primary_tail
fn primary(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(atom, primary_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::Call(args, tail) => {
                        let s = current_expr.span().till(&args);
                        (Rc::new(Expression::Call(current_expr, args, s)), tail)
                    }
                    IncompleteExpression::Slice(slice, tail) => {
                        let s = current_expr.span().till(&slice);
                        (Rc::new(Expression::Slice(current_expr, slice, s)), tail)
                    }
                    IncompleteExpression::Subscript(name, tail) => {
                        let s = current_expr.span().till(&name);
                        (Rc::new(Expression::Subscript(current_expr, name, s)), tail)
                    }
                    IncompleteExpression::PrimaryGenexp(genexp, tail) => {
                        let s = current_expr.span().till(&genexp);
                        (
                            Rc::new(Expression::PrimaryGenexp(current_expr, *genexp, s)),
                            tail,
                        )
                    }
                    IncompleteExpression::Invalid => (
                        Rc::new(Expression::Invalid(current_expr.span())),
                        Box::new(IncompleteExpression::Empty),
                    ),
                    _ => unreachable!(),
                };
            }
            current_expr
        })
        .parse(input)
}

// primary_tail:
//     | '.' NAME primary_tail
//     | '(' [arguments] ')' primary_tail
//     | '[' slices ']' primary_tail
//     | genexp primary_tail
//     | epsilon
fn primary_tail(input: ParserInput) -> ParseResult<IncompleteExpression> {
    pair(right(tok(TT::DOT), name), primary_tail)
        .map(|(n, tail)| IncompleteExpression::Subscript(n, Box::new(tail)))
        .or(pair(
            right(tok(TT::LPAR), left(maybe(arguments), tok(TT::RPAR))),
            primary_tail,
        )
        .map(|(a, tail)| {
            let args = match a.as_deref() {
                Some(Expression::Arguments(args, _)) => args.clone(),
                None => Arguments::empty(),
                _ => return IncompleteExpression::Invalid,
            };
            IncompleteExpression::Call(args, Box::new(tail))
        }))
        .or(pair(
            right(tok(TT::LSQB), left(slices, tok(TT::RSQB))),
            primary_tail,
        )
        .map(|(n, tail)| IncompleteExpression::Slice(n, Box::new(tail))))
        .or(pair(genexp, primary_tail)
            .map(|(g, tail)| IncompleteExpression::PrimaryGenexp(Box::new(g), Box::new(tail))))
        .or(epsilon.map(|_| IncompleteExpression::Empty))
        .parse(input)
}

// slices:
//     | slice !','
//     | ','.(slice | starred_expression)+ [',']
fn slices(input: ParserInput) -> ParseResult<Vec<Slice>> {
    left(slice, not(tok(TT::COMMA)))
        .map(|s| vec![s])
        .or(left(
            sep_by(slice.or(starred_expression.map(|e| e.into())), TT::COMMA),
            maybe(tok(TT::COMMA)),
        ))
        .parse(input)
}

// slice:
//     | [expression] ':' [expression] [':' [expression] ]
//     | named_expression
fn slice(input: ParserInput) -> ParseResult<Slice> {
    pair(
        pair(left(maybe(expression), tok(TT::COLON)), maybe(expression)),
        maybe(right(tok(TT::COLON), maybe(expression))),
    )
    .map(|((l, m), r)| Slice::Delimited(l, m, r.flatten()))
    .or(named_expression.map(|e| e.into()))
    .parse(input)
}

// atom:
//     | NAME
//     | 'True'
//     | 'False'
//     | 'None'
//     | strings
//     | NUMBER
//     | (tuple | group | genexp)
//     | (list | listcomp)
//     | (dict | set | dictcomp | setcomp)
//     | '...'
fn atom(input: ParserInput) -> ParseResult<Rc<Expression>> {
    name.map(|n| {
        let s = n.span();
        Rc::new(Expression::Name(n, s))
    })
    .or(token_nodiscard(TT::KEYWORD, "True").map(|t| Rc::new(Expression::True(t.span))))
    .or(token_nodiscard(TT::KEYWORD, "False").map(|t| Rc::new(Expression::False(t.span))))
    .or(token_nodiscard(TT::KEYWORD, "None").map(|t| Rc::new(Expression::None(t.span))))
    .or(strings)
    .or(number)
    .or(tuple.or(group).or(genexp))
    .or(list.or(listcomp))
    .or(dict.or(set).or(dictcomp).or(setcomp))
    .or(tok(TT::ELLIPSIS).map(|t| Rc::new(Expression::Ellipsis(t.span))))
    .parse(input)
}

fn number(input: ParserInput) -> ParseResult<Rc<Expression>> {
    tok(TT::NUMBER)
        .map(|t| {
            let s = t.span;
            Rc::new(Expression::Number(t.into(), s))
        })
        .parse(input)
}

// group:
//     | '(' (yield_expr | named_expression) ')'
fn group(input: ParserInput) -> ParseResult<Rc<Expression>> {
    right(
        tok(TT::LPAR),
        left(yield_expr.or(named_expression), tok(TT::RPAR)),
    )
    .or(on_error_pass(invalid_group))
    .parse(input)
}

// # Lambda functions
// # ----------------

// lambdef:
//     | 'lambda' [lambda_params] ':' expression
fn lambdef(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        left(
            pair(
                token_nodiscard(TT::KEYWORD, "lambda"),
                maybe(lambda_parameters),
            ),
            tok(TT::COLON),
        ),
        expression,
    )
    .map(|((t, p), b)| {
        let s = t.span.till(&b);
        match p.as_deref() {
            Some(Expression::Parameters(p, _)) => Rc::new(Expression::Lambda(p.clone(), b, s)),
            None => Rc::new(Expression::Lambda(vec![], b, s)),
            _ => Rc::new(Expression::Invalid(s)),
        }
    })
    .parse(input)
}

// lambda_params:
//     | lambda_parameters
//
// # lambda_parameters etc. duplicates parameters but without annotations
// # or type comments, and if there's no comma after a parameter, we expect
// # a colon, not a close parenthesis.  (For more, see parameters above.)
// #
// lambda_parameters:
//     | lambda_slash_no_default lambda_param_no_default* lambda_param_with_default* [lambda_star_etc]
//     | lambda_slash_with_default lambda_param_with_default* [lambda_star_etc]
//     | lambda_param_no_default+ lambda_param_with_default* [lambda_star_etc]
//     | lambda_param_with_default+ [lambda_star_etc]
//     | lambda_star_etc
fn lambda_parameters(input: ParserInput) -> ParseResult<Rc<Expression>> {
    on_error_pass(invalid_lambda_parameters)
        .or(pair(
            pair(
                lambda_slash_no_default,
                zero_or_more(lambda_param_no_default),
            ),
            pair(
                zero_or_more(lambda_param_with_default),
                maybe(lambda_star_etc),
            ),
        )
        .map(|((mut p, q), (r, s))| {
            let u = p.span().till(&r);
            p.extend(q);
            p.extend(r);
            match s.as_deref() {
                Some(Expression::Parameters(t, _)) => p.extend(t.clone()),
                None => {}
                _ => return Rc::new(Expression::Invalid(u)),
            }
            Rc::new(Expression::Parameters(p, u))
        })
        .or(pair(
            pair(
                lambda_slash_with_default,
                zero_or_more(lambda_param_with_default),
            ),
            maybe(lambda_star_etc),
        )
        .map(|((mut p, q), r)| {
            let u = p.span().till(&q);
            p.extend(q);
            match r.as_deref() {
                Some(Expression::Parameters(s, _)) => p.extend(s.clone()),
                None => {}
                _ => return Rc::new(Expression::Invalid(u)),
            }
            Rc::new(Expression::Parameters(p, u))
        }))
        .or(pair(
            pair(
                one_or_more(lambda_param_no_default),
                zero_or_more(lambda_param_with_default),
            ),
            maybe(lambda_star_etc),
        )
        .map(|((mut p, q), r)| {
            let u = p.span().till(&q);
            p.extend(q);
            match r.as_deref() {
                Some(Expression::Parameters(s, _)) => p.extend(s.clone()),
                None => {}
                _ => return Rc::new(Expression::Invalid(u)),
            }
            Rc::new(Expression::Parameters(p, u))
        }))
        .or(pair(
            one_or_more(lambda_param_with_default),
            maybe(lambda_star_etc),
        )
        .map(|(mut p, q)| {
            let u = p.span();
            match q.as_deref() {
                Some(Expression::Parameters(s, _)) => p.extend(s.clone()),
                None => {}
                _ => return Rc::new(Expression::Invalid(u)),
            }
            Rc::new(Expression::Parameters(p, u))
        })))
        .or(lambda_star_etc)
        .parse(input)
}

// lambda_slash_no_default:
//     | lambda_param_no_default+ '/' ','
//     | lambda_param_no_default+ '/' &':'
fn lambda_slash_no_default(input: ParserInput) -> ParseResult<Vec<Parameter>> {
    left(
        left(one_or_more(lambda_param_no_default), tok(TT::SLASH)),
        tok(TT::COMMA).discard().or(lookahead(tok(TT::COLON))),
    )
    .parse(input)
}

// lambda_slash_with_default:
//     | lambda_param_no_default* lambda_param_with_default+ '/' ','
//     | lambda_param_no_default* lambda_param_with_default+ '/' &':'
fn lambda_slash_with_default(input: ParserInput) -> ParseResult<Vec<Parameter>> {
    left(
        left(
            pair(
                zero_or_more(lambda_param_no_default),
                one_or_more(lambda_param_with_default),
            ),
            tok(TT::SLASH),
        ),
        tok(TT::COMMA).discard().or(lookahead(tok(TT::COLON))),
    )
    .map(|(mut p, q)| {
        p.extend(q);
        p
    })
    .parse(input)
}

// lambda_star_etc:
//     | '*' lambda_param_no_default lambda_param_maybe_default* [lambda_kwds]
//     | '*' ',' lambda_param_maybe_default+ [lambda_kwds]
//     | lambda_kwds
fn lambda_star_etc(input: ParserInput) -> ParseResult<Rc<Expression>> {
    on_error_pass(invalid_lambda_star_etc)
        .or(pair(
            right(tok(TT::STAR), lambda_param_no_default),
            pair(zero_or_more(lambda_param_maybe_default), maybe(lambda_kwds)),
        )
        .map(|(mut p, (mut q, r))| {
            let u = p.span().till(&q).or(&r);
            p.starred = true;
            q.insert(0, p);
            if let Some(kwds) = r {
                match kwds.as_ref() {
                    Expression::Parameters(ps, _) => q.extend(ps.clone()),
                    _ => return Rc::new(Expression::Invalid(u)),
                }
            }
            Rc::new(Expression::Parameters(q, u))
        }))
        .or(pair(
            right(
                pair(tok(TT::STAR), tok(TT::COMMA)),
                one_or_more(lambda_param_maybe_default),
            ),
            maybe(lambda_kwds),
        )
        .map(|(mut p, q)| {
            let u = p.span().till(&p).or(&q);
            if let Some(kwds) = q.as_deref() {
                match kwds {
                    Expression::Parameters(ps, _) => p.extend(ps.clone()),
                    _ => return Rc::new(Expression::Invalid(u)),
                }
            }
            Rc::new(Expression::Parameters(p, u))
        }))
        .or(lambda_kwds.map(|k| match k.as_ref() {
            Expression::Parameters(_, _) => return k,
            _ => return Rc::new(Expression::Invalid(k.span())),
        }))
        .parse(input)
}

// lambda_kwds:
//     | '**' lambda_param_no_default
fn lambda_kwds(input: ParserInput) -> ParseResult<Rc<Expression>> {
    on_error_pass(invalid_lambda_kwds)
        .or(
            right(tok(TT::DOUBLESTAR), lambda_param_no_default).map(|param| {
                let span = param.span();
                Rc::new(Expression::Parameters(vec![param], span))
            }),
        )
        .parse(input)
}

// lambda_param_no_default:
//     | lambda_param ','
//     | lambda_param &':'
fn lambda_param_no_default(input: ParserInput) -> ParseResult<Parameter> {
    left(
        lambda_param,
        tok(TT::COMMA).discard().or(lookahead(tok(TT::COLON))),
    )
    .parse(input)
}

// lambda_param_with_default:
//     | lambda_param default ','
//     | lambda_param default &':'
fn lambda_param_with_default(input: ParserInput) -> ParseResult<Parameter> {
    left(
        pair(lambda_param, default),
        tok(TT::COMMA).discard().or(lookahead(tok(TT::COLON))),
    )
    .map(|(mut l, d)| {
        l.default = Some(d);
        l
    })
    .parse(input)
}

// lambda_param_maybe_default:
//     | lambda_param default? ','
//     | lambda_param default? &':'
fn lambda_param_maybe_default(input: ParserInput) -> ParseResult<Parameter> {
    left(
        pair(lambda_param, maybe(default)),
        tok(TT::COMMA).discard().or(lookahead(tok(TT::COLON))),
    )
    .map(|(mut l, d)| {
        l.default = d;
        l
    })
    .parse(input)
}

// lambda_param: NAME
fn lambda_param(input: ParserInput) -> ParseResult<Parameter> {
    name.map(|n| n.into()).parse(input)
}

// # LITERALS
// # ========

// fstring_middle:
//     | fstring_replacement_field
//     | FSTRING_MIDDLE
fn fstring_middle(input: ParserInput) -> ParseResult<Rc<Expression>> {
    fstring_replacement_field
        .map(|fs| {
            if let Expression::FStringReplacement(f, s) = fs.as_ref() {
                Rc::new(Expression::FString(
                    FString::Interpolated(f.clone()),
                    s.clone(),
                ))
            } else {
                fs
            }
        })
        .or(tok(TT::FSTRING_MIDDLE).map(|f| {
            let s = f.span;
            Rc::new(Expression::FString(
                FString::Literal(Rc::from(f.lexeme), s),
                s,
            ))
        }))
        .parse(input)
}

// fstring_replacement_field:
//     | '{' (yield_expr | star_expressions) '='? [fstring_conversion] [fstring_full_format_spec] '}'
fn fstring_replacement_field(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        pair(
            right(
                tok(TT::LBRACE),
                yield_expr.map(|e| vec![e]).or(star_expressions),
            ),
            pair(maybe(tok(TT::EQUAL)), maybe(fstring_conversion)),
        ),
        left(maybe(fstring_full_format_spec), tok(TT::RBRACE)),
    )
    .map(|((exprs, (dbg, conversion)), fmt)| {
        let span = exprs.span();
        Rc::new(Expression::FStringReplacement(
            FStringReplacement {
                exprs,
                debug: dbg.is_some(),
                conversion,
                format_specs: fmt.unwrap_or_default(),
            },
            span,
        ))
    })
    .or(invalid_replacement_field)
    .parse(input)
}

// fstring_conversion:
//     | "!" NAME
fn fstring_conversion(input: ParserInput) -> ParseResult<Name> {
    right(tok(TT::EXCLAMATION), name).parse(input)
}

// fstring_full_format_spec:
//     | ':' fstring_format_spec*
fn fstring_full_format_spec(input: ParserInput) -> ParseResult<Vec<Rc<Expression>>> {
    right(tok(TT::COLON), zero_or_more(fstring_format_spec)).parse(input)
}

// fstring_format_spec:
//     | FSTRING_MIDDLE
//     | fstring_replacement_field
fn fstring_format_spec(input: ParserInput) -> ParseResult<Rc<Expression>> {
    tok(TT::FSTRING_MIDDLE)
        .map(|t| {
            Rc::new(Expression::Strings(
                vec![PyString::Literal(Rc::from(t.lexeme), t.span)],
                t.span,
            ))
        })
        .or(fstring_replacement_field)
        .parse(input)
}

// fstring:
//     | FSTRING_START fstring_middle* FSTRING_END
fn fstring(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(
        right(tok(TT::FSTRING_START), zero_or_more(fstring_middle)),
        tok(TT::FSTRING_END),
    )
    .map(|sv| {
        let span = sv.span();
        if sv
            .iter()
            .all(|s| matches!(s.as_ref(), Expression::FString(_, _)))
        {
            todo!()
        } else {
            Rc::new(Expression::Invalid(span))
        }
    })
    .parse(input)
}

// string: STRING
fn string(input: ParserInput) -> ParseResult<Token> {
    tok(TT::STRING).parse(input)
}

// strings: (fstring|string)+
fn strings(input: ParserInput) -> ParseResult<Rc<Expression>> {
    one_or_more(
        string.map(|t| PyString::Literal(Rc::from(t.lexeme), t.span)), // .or(fstring), FIXME
    )
    .map(|t| {
        let s = t.span();
        Rc::new(Expression::Strings(t, s))
    })
    .parse(input)
}

// list:
//     | '[' [star_named_expressions] ']'
fn list(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        pair(tok(TT::LSQB), maybe(star_named_expressions)),
        tok(TT::RSQB),
    )
    .map(|((l, e), r)| {
        let s = l.span.till(&r);
        Rc::new(Expression::List(e.unwrap_or_default(), s))
    })
    .parse(input)
}

// tuple:
//     | '(' [star_named_expression ',' [star_named_expressions]  ] ')'
fn tuple(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        pair(
            tok(TT::LPAR),
            maybe(pair(
                left(star_named_expression, tok(TT::COMMA)),
                maybe(star_named_expressions),
            )),
        ),
        tok(TT::RPAR),
    )
    .map(|((l, t), r)| {
        let s = l.span.till(&r);
        let mut v = Vec::new();
        if let Some((e, es)) = t {
            v.push(e);
            v.extend(es.unwrap_or_default());
        }
        Rc::new(Expression::Tuple(v, s))
    })
    .parse(input)
}

// set: '{' star_named_expressions '}'
fn set(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        pair(tok(TT::LBRACE), star_named_expressions),
        tok(TT::RBRACE),
    )
    .map(|((l, e), r)| {
        let s = l.span.till(&r);
        Rc::new(Expression::Set(e, s))
    })
    .parse(input)
}

// # Dicts
// # -----

// dict:
//     | '{' [double_starred_kvpairs] '}'
fn dict(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        pair(tok(TT::LBRACE), maybe(double_starred_kvpairs)),
        tok(TT::RBRACE),
    )
    .map(|((l, e), r)| {
        let s = l.span.till(&r);
        Rc::new(Expression::Dict(e.unwrap_or_default(), s))
    })
    .or(on_error_pass(left(
        right(tok(TT::LBRACE), invalid_double_starred_kvpairs),
        tok(TT::RBRACE),
    )))
    .parse(input)
}

// double_starred_kvpairs: ','.double_starred_kvpair+ [',']
fn double_starred_kvpairs(input: ParserInput) -> ParseResult<Vec<Rc<Expression>>> {
    left(
        sep_by(double_starred_kvpair, TT::COMMA),
        maybe(tok(TT::COMMA)),
    )
    .parse(input)
}

// double_starred_kvpair:
//     | '**' bitwise_or
//     | kvpair
fn double_starred_kvpair(input: ParserInput) -> ParseResult<Rc<Expression>> {
    right(tok(TT::DOUBLESTAR), bitwise_or)
        .map(|e| {
            let s = e.span();
            Rc::new(Expression::DictUnwrap(e, s))
        })
        .or(kvpair)
        .parse(input)
}

// kvpair: expression ':' expression
fn kvpair(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(expression, right(tok(TT::COLON), expression))
        .map(|(e, f)| {
            let s = e.span().till(&f);
            Rc::new(Expression::Tuple(vec![e, f], s))
        })
        .parse(input)
}

// # Comprehensions & Generators
// # ---------------------------

// for_if_clauses:
//     | for_if_clause+
fn for_if_clauses(input: ParserInput) -> ParseResult<Vec<Rc<Expression>>> {
    one_or_more(for_if_clause).parse(input)
}

// for_if_clause:
//     | ASYNC 'for' star_targets 'in' ~ disjunction ('if' disjunction )*
//     | 'for' star_targets 'in' ~ disjunction ('if' disjunction )*
fn for_if_clause(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        maybe(token(TT::KEYWORD, "async")),
        pair(
            pair(
                pair(token_nodiscard(TT::KEYWORD, "for"), star_targets),
                right(
                    token(TT::KEYWORD, "in").expect(move || {
                        input.report_error(Error::new(
                            input.next_span(),
                            "'in' expected after for-loop variables",
                        ))
                    }),
                    disjunction,
                ),
            ),
            zero_or_more(right(token(TT::KEYWORD, "if"), disjunction)),
        ),
    )
    .map(|(a, (((t, tgt), set), ifs))| {
        let s = t.span.till(&set);
        Rc::new(Expression::ForIfClause(tgt, set, ifs, a.is_some(), s))
    })
    .or(on_error_pass(
        invalid_for_target.map(|s| Rc::new(Expression::Invalid(s.span()))),
    ))
    .parse(input)
}

// listcomp:
//     | '[' named_expression for_if_clauses ']'
fn listcomp(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        right(tok(TT::LSQB), star_named_expression),
        left(for_if_clauses, tok(TT::RSQB)),
    )
    .map(|(e, f)| {
        let s = e.span().till(&f);
        Rc::new(Expression::ListComprehension(e, f, s))
    })
    .or(on_error_pass(invalid_comprehension))
    .parse(input)
}

// setcomp:
//     | '{' named_expression for_if_clauses '}'
fn setcomp(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        right(tok(TT::LBRACE), named_expression),
        left(for_if_clauses, tok(TT::RBRACE)),
    )
    .map(|(e, f)| {
        let s = e.span().till(&f);
        Rc::new(Expression::SetComprehension(e, f, s))
    })
    .or(on_error_pass(invalid_comprehension))
    .parse(input)
}

// genexp:
//     | '(' ( assignment_expression | expression !':=') for_if_clauses ')'
fn genexp(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        right(
            tok(TT::LPAR),
            assignment_expression.or(left(expression, not(tok(TT::COLONEQUAL)))),
        ),
        left(for_if_clauses, tok(TT::RPAR)),
    )
    .map(|(e, f)| {
        let s = e.span().till(&f);
        Rc::new(Expression::Generator(e, f, s))
    })
    .or(on_error_pass(invalid_comprehension))
    .parse(input)
}

// dictcomp:
//     | '{' kvpair for_if_clauses '}'
fn dictcomp(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        right(tok(TT::LBRACE), kvpair),
        left(for_if_clauses, tok(TT::RSQB)),
    )
    .map(|(e, f)| {
        let s = e.span().till(&f);
        Rc::new(Expression::DictComprehension(e, f, s))
    })
    .or(on_error_pass(invalid_dict_comprehension))
    .parse(input)
}

// # FUNCTION CALL ARGUMENTS
// # =======================

// arguments:
//     | args [','] &')'
fn arguments(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(
        args.map(|a| {
            let s = a.span();
            Rc::new(Expression::Arguments(a, s))
        }),
        pair(maybe(tok(TT::COMMA)), lookahead(tok(TT::RPAR))),
    )
    .or(on_error_pass(invalid_arguments))
    .parse(input)
}

// args:
//     | ','.(starred_expression | ( assignment_expression | expression !':=') !'=')+ [',' kwargs ]
//     | kwargs
fn args(input: ParserInput) -> ParseResult<Arguments> {
    pair(
        sep_by(
            starred_expression.or(left(
                assignment_expression.or(left(expression, not(tok(TT::COLONEQUAL)))),
                not(tok(TT::EQUAL)),
            )),
            TT::COMMA,
        ),
        maybe(right(tok(TT::COMMA), kwargs)),
    )
    .map(|(pos, kw)| Arguments {
        positional: pos.to_vec(),
        keyword: kw.unwrap_or_default(),
    })
    .or(kwargs.map(|args| Arguments {
        positional: vec![],
        keyword: args,
    }))
    .parse(input)
}

// kwargs:
//     | ','.kwarg_or_starred+ ',' ','.kwarg_or_double_starred+
//     | ','.kwarg_or_starred+
//     | ','.kwarg_or_double_starred+
fn kwargs(input: ParserInput) -> ParseResult<Vec<Rc<Expression>>> {
    pair(
        left(sep_by(kwarg_or_starred, TT::COMMA), tok(TT::COMMA)),
        sep_by(kwarg_or_double_starred, TT::COMMA),
    )
    .map(|(mut u, v)| {
        u.extend(v);
        u
    })
    .or(sep_by(kwarg_or_starred, TT::COMMA))
    .or(sep_by(kwarg_or_double_starred, TT::COMMA))
    .parse(input)
}

// starred_expression:
//     | '*' expression
fn starred_expression(input: ParserInput) -> ParseResult<Rc<Expression>> {
    on_error_pass(invalid_starred_expression)
        .or(right(tok(TT::STAR), expression).map(|e| {
            let s = e.span();
            Rc::new(Expression::ListUnwrap(e, s))
        }))
        .parse(input)
}

// kwarg_or_starred:
//     | NAME '=' expression
//     | starred_expression
fn kwarg_or_starred(input: ParserInput) -> ParseResult<Rc<Expression>> {
    on_error_pass(invalid_kwarg)
        .or(pair(left(name, tok(TT::EQUAL)), expression)
            .map(|(n, e)| {
                let s = n.span.till(&e);
                Rc::new(Expression::KeywordArgument(n, e, s))
            })
            .or(starred_expression))
        .parse(input)
}

// kwarg_or_double_starred:
//     | NAME '=' expression
//     | '**' expression
fn kwarg_or_double_starred(input: ParserInput) -> ParseResult<Rc<Expression>> {
    on_error_pass(invalid_kwarg)
        .or(pair(left(name, tok(TT::EQUAL)), expression)
            .map(|(n, e)| {
                let s = n.span.till(&e);
                Rc::new(Expression::KeywordArgument(n, e, s))
            })
            .or(right(tok(TT::DOUBLESTAR), expression)))
        .parse(input)
}

// # ASSIGNMENT TARGETS
// # ==================

// # Generic targets
// # ---------------

// # NOTE: star_targets may contain *bitwise_or, targets may not.
// star_targets:
//     | star_target !','
//     | star_target (',' star_target )* [',']
fn star_targets(input: ParserInput) -> ParseResult<Vec<Rc<Expression>>> {
    left(sep_by(star_target, TT::COMMA), maybe(tok(TT::COMMA))).parse(input)
}

// star_targets_list_seq: ','.star_target+ [',']
fn star_targets_list_seq(input: ParserInput) -> ParseResult<Vec<Rc<Expression>>> {
    left(sep_by(star_target, TT::COMMA), maybe(tok(TT::COMMA))).parse(input)
}

// star_targets_tuple_seq:
//     | star_target (',' star_target )+ [',']
//     | star_target ','
fn star_targets_tuple_seq(input: ParserInput) -> ParseResult<Vec<Rc<Expression>>> {
    pair(
        star_target,
        left(
            one_or_more(right(tok(TT::COMMA), star_target)),
            maybe(tok(TT::COMMA)),
        )
        .or(tok(TT::COMMA).map(|_| vec![])),
    )
    .map(|(u, mut v)| {
        v.insert(0, u);
        v
    })
    .parse(input)
}

// star_target:
//     | '*' (!'*' star_target)
//     | target_with_star_atom
fn star_target(input: ParserInput) -> ParseResult<Rc<Expression>> {
    if let Some(result) = try_remember(input, star_target) {
        return result;
    }
    let result = right(tok(TT::STAR), right(not(tok(TT::STAR)), star_target))
        .or(target_with_star_atom)
        .parse(input);
    save_result(input, star_target, &result);
    result
}

// target_with_star_atom:
//     | t_primary '.' NAME !t_lookahead
//     | t_primary '[' slices ']' !t_lookahead
//     | star_atom
fn target_with_star_atom(input: ParserInput) -> ParseResult<Rc<Expression>> {
    if let Some(result) = try_remember(input, target_with_star_atom) {
        return result;
    }
    let result = pair(
        t_primary,
        left(
            right(tok(TT::DOT), name.map(Selector::Name)),
            not(t_lookahead),
        )
        .or(left(
            right(
                tok(TT::LSQB),
                left(slices.map(Selector::Slice), tok(TT::RSQB)),
            ),
            not(t_lookahead),
        )),
    )
    .map(|(p, s)| s.apply_to(p))
    .or(star_atom)
    .parse(input);
    save_result(input, target_with_star_atom, &result);
    result
}

// star_atom:
//     | NAME
//     | '(' target_with_star_atom ')'
//     | '(' [star_targets_tuple_seq] ')'
//     | '[' [star_targets_list_seq] ']'
fn star_atom(input: ParserInput) -> ParseResult<Rc<Expression>> {
    name.map(|n| {
        let s = n.span;
        Rc::new(Expression::Name(n, s))
    })
    .or(left(
        right(tok(TT::LPAR), target_with_star_atom),
        tok(TT::RPAR),
    ))
    .or(pair(
        pair(tok(TT::LPAR), maybe(star_targets_tuple_seq)),
        tok(TT::RPAR),
    )
    .map(|((l, v), r)| {
        let s = l.span.till(&r);
        Rc::new(Expression::Tuple(v.unwrap_or_default(), s))
    }))
    .or(pair(
        pair(tok(TT::LSQB), maybe(star_targets_list_seq)),
        tok(TT::RSQB),
    )
    .map(|((l, v), r)| {
        let s = l.span.till(&r);
        Rc::new(Expression::List(v.unwrap_or_default(), s))
    }))
    .parse(input)
}

// single_target:
//     | single_subscript_attribute_target
//     | NAME
//     | '(' single_target ')'
fn single_target(input: ParserInput) -> ParseResult<Rc<Expression>> {
    single_subscript_attribute_target
        .or(name.map(|n| {
            let s = n.span;
            Rc::new(Expression::Name(n, s))
        }))
        .or(left(right(tok(TT::LPAR), single_target), tok(TT::RPAR)))
        .parse(input)
}

enum Selector {
    Name(Name),
    Slice(Vec<Slice>),
}

impl Selector {
    fn apply_to(self, expr: Rc<Expression>) -> Rc<Expression> {
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

// single_subscript_attribute_target:
//     | t_primary '.' NAME !t_lookahead
//     | t_primary '[' slices ']' !t_lookahead
fn single_subscript_attribute_target(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        t_primary,
        right(
            tok(TT::DOT),
            left(name.map(Selector::Name), not(t_lookahead)),
        )
        .or(right(
            tok(TT::LSQB),
            left(
                left(slices.map(Selector::Slice), tok(TT::RSQB)),
                not(t_lookahead),
            ),
        )),
    )
    .map(|(p, s)| s.apply_to(p))
    .parse(input)
}

// t_primary:
//     | atom &t_lookahead t_primary_tail
fn t_primary(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(left(atom, lookahead(t_lookahead)), t_primary_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::Call(args, tail) => {
                        let s = current_expr.span().till(&args);
                        (Rc::new(Expression::Call(current_expr, args, s)), tail)
                    }
                    IncompleteExpression::Slice(slice, tail) => {
                        let s = current_expr.span().till(&slice);
                        (Rc::new(Expression::Slice(current_expr, slice, s)), tail)
                    }
                    IncompleteExpression::Subscript(name, tail) => {
                        let s = current_expr.span().till(&name);
                        (Rc::new(Expression::Subscript(current_expr, name, s)), tail)
                    }
                    IncompleteExpression::PrimaryGenexp(genexp, tail) => {
                        let s = current_expr.span().till(&genexp);
                        (
                            Rc::new(Expression::PrimaryGenexp(current_expr, *genexp, s)),
                            tail,
                        )
                    }
                    IncompleteExpression::Invalid => (
                        Rc::new(Expression::Invalid(current_expr.span())),
                        Box::new(IncompleteExpression::Empty),
                    ),
                    _ => unreachable!(),
                };
            }
            current_expr
        })
        .parse(input)
}

// t_primary_tail:
//     | '.' NAME &t_lookahead t_primary_tail
//     | '[' slices ']' &t_lookahead t_primary_tail
//     | genexp &t_lookahead t_primary_tail
//     | '(' [arguments] ')' &t_lookahead t_primary_tail
//     | epsilon
fn t_primary_tail(input: ParserInput) -> ParseResult<IncompleteExpression> {
    pair(
        right(tok(TT::DOT), left(name, lookahead(t_lookahead))),
        t_primary_tail,
    )
    .map(|(n, tail)| IncompleteExpression::Subscript(n, Box::new(tail)))
    .or(pair(
        right(
            tok(TT::LSQB),
            left(slices, pair(tok(TT::RSQB), lookahead(t_lookahead))),
        ),
        t_primary_tail,
    )
    .map(|(s, tail)| IncompleteExpression::Slice(s, Box::new(tail))))
    .or(pair(left(genexp, lookahead(t_lookahead)), t_primary_tail)
        .map(|(g, tail)| IncompleteExpression::PrimaryGenexp(Box::new(g), Box::new(tail))))
    .or(pair(
        right(
            tok(TT::LPAR),
            left(arguments, pair(tok(TT::RPAR), lookahead(t_lookahead))),
        ),
        t_primary_tail,
    )
    .map(|(n, tail)| {
        let args = match n.as_ref() {
            Expression::Arguments(args, _) => args.clone(),
            _ => return IncompleteExpression::Invalid,
        };
        IncompleteExpression::Call(args, Box::new(tail))
    }))
    .or(epsilon.map(|_| IncompleteExpression::Empty))
    .parse(input)
}

// t_lookahead: '(' | '[' | '.'
fn t_lookahead(input: ParserInput) -> ParseResult<Token> {
    tok(TT::LPAR)
        .or(tok(TT::LSQB))
        .or(tok(TT::DOT))
        .parse(input)
}

// # Targets for del statements
// # --------------------------

// del_targets: ','.del_target+ [',']
fn del_targets(input: ParserInput) -> ParseResult<Vec<Rc<Expression>>> {
    left(sep_by(del_target, TT::COMMA), maybe(tok(TT::COMMA))).parse(input)
}

// del_target:
//     | t_primary !t_lookahead
//     | del_t_atom
fn del_target(input: ParserInput) -> ParseResult<Rc<Expression>> {
    if let Some(result) = try_remember(input, del_target) {
        return result;
    }
    let result = left(t_primary, not(t_lookahead))
        .or(del_t_atom)
        .parse(input);
    save_result(input, del_target, &result);
    result
}

// del_t_atom:
//     | NAME
//     | '(' del_target ')'
//     | '(' [del_targets] ')'
//     | '[' [del_targets] ']'
fn del_t_atom(input: ParserInput) -> ParseResult<Rc<Expression>> {
    name.map(|n| {
        let s = n.span;
        Rc::new(Expression::Name(n, s))
    })
    .or(left(right(tok(TT::LPAR), del_target), tok(TT::RPAR)))
    .or(
        pair(pair(tok(TT::LPAR), maybe(del_targets)), tok(TT::RPAR)).map(|((l, v), r)| {
            let s = l.span.till(&r);
            Rc::new(Expression::Tuple(v.unwrap_or_default(), s))
        }),
    )
    .or(
        pair(pair(tok(TT::LSQB), maybe(del_targets)), tok(TT::RSQB)).map(|((l, v), r)| {
            let s = l.span.till(&r);
            Rc::new(Expression::List(v.unwrap_or_default(), s))
        }),
    )
    .parse(input)
}

// # TYPING ELEMENTS
// # ---------------

// # type_expressions allow */** but ignore them
// type_expressions:
//     | ','.expression+ ',' '*' expression ',' '**' expression
//     | ','.expression+ ',' '*' expression
//     | ','.expression+ ',' '**' expression
//     | '*' expression ',' '**' expression
//     | '*' expression
//     | '**' expression
//     | ','.expression+

// func_type_comment:
//     | NEWLINE TYPE_COMMENT &(NEWLINE INDENT)   # Must be followed by indented block
//     | invalid_double_type_comments
//     | TYPE_COMMENT

// # ========================= END OF THE GRAMMAR ===========================

// # ========================= START OF INVALID RULES =======================

// # From here on, there are rules for invalid syntax with specialised error messages
// invalid_arguments:
//     | ((','.(starred_expression | ( assignment_expression | expression !':=') !'=')+ ',' kwargs) | kwargs) ',' b='*' {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(b, "iterable argument unpacking follows keyword argument unpacking") }
//     | a=expression b=for_if_clauses ',' [args | expression for_if_clauses] {
//         RAISE_SYNTAX_ERROR_KNOWN_RANGE(a, _PyPegen_get_last_comprehension_item(PyPegen_last_item(b, comprehension_ty)), "Generator expression must be parenthesized") }
//     | a=NAME b='=' expression for_if_clauses {
//         RAISE_SYNTAX_ERROR_KNOWN_RANGE(a, b, "invalid syntax. Maybe you meant '==' or ':=' instead of '='?")}
//     | (args ',')? a=NAME b='=' &(',' | ')') {
//         RAISE_SYNTAX_ERROR_KNOWN_RANGE(a, b, "expected argument value expression")}
//     | a=args b=for_if_clauses { _PyPegen_nonparen_genexp_in_call(p, a, b) }
//     | args ',' a=expression b=for_if_clauses {
//         RAISE_SYNTAX_ERROR_KNOWN_RANGE(a, _PyPegen_get_last_comprehension_item(PyPegen_last_item(b, comprehension_ty)), "Generator expression must be parenthesized") }
//     | a=args ',' args { _PyPegen_arguments_parsing_error(p, a) }
fn invalid_arguments(input: ParserInput) -> ParseResult<Rc<Expression>> {
    right(
        sep_by(
            starred_expression.discard().or(pair(
                assignment_expression
                    .discard()
                    .or(pair(expression, not(tok(TT::COLONEQUAL))).discard()),
                not(tok(TT::EQUAL)),
            )
            .discard()),
            TT::COMMA,
        ),
        right(tok(TT::COMMA), tok(TT::STAR)),
    )
    .map(move |a| {
        let error = Error::with_underline(
            a.span(),
            "iterable argument unpacking follows keyword argument unpacking",
        );
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    })
    .or(left(
        pair(expression, for_if_clauses),
        pair(
            tok(TT::COMMA),
            maybe(
                args.discard()
                    .or(pair(expression, for_if_clauses).discard()),
            ),
        ),
    )
    .map(move |(a, b)| {
        let error = Error::with_range(
            a.span(),
            b.span(),
            "Generator expression must be parenthesized",
        );
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    }))
    .or(
        left(pair(name, tok(TT::EQUAL)), pair(expression, for_if_clauses)).map(move |(a, b)| {
            let error = Error::with_range(
                a.span(),
                b.span(),
                "invalid syntax. Maybe you meant '==' or ':=' instead of '='?",
            );
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }),
    )
    .or(right(
        maybe(pair(args, tok(TT::COMMA))),
        left(
            pair(name, tok(TT::EQUAL)),
            lookahead(tok(TT::COMMA).or(tok(TT::RPAR))),
        ),
    )
    .map(move |(a, b)| {
        let error = Error::with_range(a.span(), b.span(), "expected argument value expression");
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    }))
    //     FIXME: | a=args b=for_if_clauses { _PyPegen_nonparen_genexp_in_call(p, a, b) }
    .or(
        right(pair(args, tok(TT::COMMA)), pair(expression, for_if_clauses)).map(move |(a, b)| {
            let error = Error::with_range(
                a.span(),
                b.span(),
                "Generator expression must be parenthesized",
            );
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }),
    )
    //  FIXME:   | a=args ',' args { _PyPegen_arguments_parsing_error(p, a) }
    .parse(input)
}

// invalid_kwarg:
//     | a[Token*]=('True'|'False'|'None') b='=' {
//         RAISE_SYNTAX_ERROR_KNOWN_RANGE(a, b, "cannot assign to %s", PyBytes_AS_STRING(a->bytes)) }
//     | a=NAME b='=' expression for_if_clauses {
//         RAISE_SYNTAX_ERROR_KNOWN_RANGE(a, b, "invalid syntax. Maybe you meant '==' or ':=' instead of '='?")}
//     | !(NAME '=') a=expression b='=' {
//         RAISE_SYNTAX_ERROR_KNOWN_RANGE(
//             a, b, "expression cannot contain assignment, perhaps you meant \"==\"?") }
//     | a='**' expression '=' b=expression {
//         RAISE_SYNTAX_ERROR_KNOWN_RANGE(a, b, "cannot assign to keyword argument unpacking") }
fn invalid_kwarg(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        token_nodiscard(TT::KEYWORD, "True")
            .or(token_nodiscard(TT::KEYWORD, "False"))
            .or(token_nodiscard(TT::KEYWORD, "True")),
        tok(TT::EQUAL),
    )
    .map(move |(t, s)| {
        let error = Error::with_range(
            t.span(),
            s.span(),
            &format!("cannot assign to {}", t.lexeme),
        );
        input.report_error(error);
        Rc::new(Expression::Invalid(t.span().till(&s)))
    })
    .or(
        left(pair(name, tok(TT::EQUAL)), pair(expression, for_if_clauses)).map(move |(t, s)| {
            let error = Error::with_range(
                t.span(),
                s.span(),
                "invalid syntax. Maybe you meant '==' or ':=' instead of '='?",
            );
            input.report_error(error);
            Rc::new(Expression::Invalid(t.span.till(&s)))
        }),
    )
    .or(right(
        not(pair(name, tok(TT::EQUAL))),
        pair(expression, expression),
    )
    .map(move |(t, s)| {
        let error = Error::with_range(
            t.span(),
            s.span(),
            "expression cannot contain assignment, perhaps you meant '=='?",
        );
        input.report_error(error);
        Rc::new(Expression::Invalid(t.span().till(&s)))
    }))
    .or(pair(
        left(tok(TT::DOUBLESTAR), expression),
        right(tok(TT::EQUAL), expression),
    )
    .map(move |(t, s)| {
        let error = Error::with_range(
            t.span(),
            s.span(),
            "cannot assign to keyword argument unpacking",
        );
        input.report_error(error);
        Rc::new(Expression::Invalid(t.span.till(&s)))
    }))
    .parse(input)
}

// # IMPORTANT: Note that the "_without_invalid" suffix causes the rule to not call invalid rules under it
// expression_without_invalid[expr_ty]:
//     | a=disjunction 'if' b=disjunction 'else' c=expression { _PyAST_IfExp(b, a, c, EXTRA) }
//     | disjunction
//     | lambdef
fn expression_without_invalid(input: ParserInput) -> ParseResult<Rc<Expression>> {
    todo!()
}

// invalid_legacy_expression:
//     | a=NAME !'(' b=star_expressions {
//         _PyPegen_check_legacy_stmt(p, a) ? RAISE_SYNTAX_ERROR_KNOWN_RANGE(a, b,
//             "Missing parentheses in call to '%U'. Did you mean %U(...)?", a->v.Name.id, a->v.Name.id) : NULL}
fn invalid_legacy_expression(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        name.pred(|n| *n.name == *"exec" || *n.name == *"print"),
        right(not(tok(TT::LPAR)), star_expressions),
    )
    .map(move |(a, b)| {
        let error = Error::with_range(
            a.span(),
            b.span(),
            &format!(
                "Missing parentheses in call to '{}'. Did you mean {}(...)?",
                a.name, a.name
            ),
        );
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span().till(&b)))
    })
    .parse(input)
}

// invalid_expression:
//     # !(NAME STRING) is not matched so we don't show this error with some invalid string prefixes like: kf"dsfsdf"
//     # Soft keywords need to also be ignored because they can be parsed as NAME NAME
//    | !(NAME STRING | SOFT_KEYWORD) a=disjunction b=expression_without_invalid {
//         _PyPegen_check_legacy_stmt(p, a) ? NULL : p->tokens[p->mark-1]->level == 0 ? NULL :
//         RAISE_SYNTAX_ERROR_KNOWN_RANGE(a, b, "invalid syntax. Perhaps you forgot a comma?") }
//    | a=disjunction 'if' b=disjunction !('else'|':') { RAISE_SYNTAX_ERROR_KNOWN_RANGE(a, b, "expected 'else' after 'if' expression") }
//    | a='lambda' [lambda_params] b=':' &FSTRING_MIDDLE  {
//         RAISE_SYNTAX_ERROR_KNOWN_RANGE(a, b, "f-string: lambda expressions are not allowed without parentheses") }
// FIXME: Incomplete
fn invalid_expression(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        left(disjunction, token(TT::KEYWORD, "if")),
        left(
            disjunction,
            not(token(TT::KEYWORD, "else").or(tok(TT::COLON).discard())),
        ),
    )
    .map(move |(a, b)| {
        let error = Error::with_range(a.span(), b.span(), "expected 'else' after 'if' expression");
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span().till(&b)))
    })
    .or(pair(
        left(
            token_nodiscard(TT::KEYWORD, "lambda"),
            maybe(lambda_parameters),
        ),
        left(tok(TT::COLON), lookahead(tok(TT::FSTRING_MIDDLE))),
    )
    .map(move |(a, b)| {
        let error = Error::with_range(
            a.span(),
            b.span(),
            "f-string: lambda expressions are not allowed without parentheses",
        );
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span().till(&b)))
    }))
    .parse(input)
}

// invalid_named_expression(memo):
//     | a=expression ':=' expression {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(
//             a, "cannot use assignment expressions with %s", _PyPegen_get_expr_name(a)) }
//     | a=NAME '=' b=bitwise_or !('='|':=') {
//         RAISE_SYNTAX_ERROR_KNOWN_RANGE(a, b, "invalid syntax. Maybe you meant '==' or ':=' instead of '='?") }
//     | !(list|tuple|genexp|'True'|'None'|'False') a=bitwise_or b='=' bitwise_or !('='|':=') {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "cannot assign to %s here. Maybe you meant '==' instead of '='?",
//                                           _PyPegen_get_expr_name(a)) }
fn invalid_named_expression(input: ParserInput) -> ParseResult<Rc<Expression>> {
    if let Some(result) = try_remember(input, invalid_named_expression) {
        return result;
    }
    let result = left(left(expression, tok(TT::COLONEQUAL)), expression)
        .map(move |t| {
            let error = Error::starting_from(t.span(), "cannot use assignment expressions here");
            input.report_error(error);
            Rc::new(Expression::Invalid(t.span()))
        })
        .or(pair(
            left(name, tok(TT::COLONEQUAL)),
            left(bitwise_or, not(tok(TT::EQUAL).or(tok(TT::COLONEQUAL)))),
        )
        .map(move |(s, t)| {
            let error = Error::with_range(
                s.span(),
                t.span(),
                "invalid syntax. Maybe you meant '==' or ':=' instead of '='?",
            );
            input.report_error(error);
            Rc::new(Expression::Invalid(s.span().till(&t)))
        }))
        .or(left(
            right(
                not(list
                    .or(tuple)
                    .or(genexp)
                    .or(token_nodiscard(TT::KEYWORD, "True")
                        .map(|t| Rc::new(Expression::True(t.span))))
                    .or(token_nodiscard(TT::KEYWORD, "None")
                        .map(|t| Rc::new(Expression::None(t.span))))
                    .or(token_nodiscard(TT::KEYWORD, "False")
                        .map(|t| Rc::new(Expression::False(t.span))))),
                pair(bitwise_or, tok(TT::EQUAL)),
            ),
            left(bitwise_or, not(tok(TT::EQUAL).or(tok(TT::COLONEQUAL)))),
        )
        .map(move |(s, t)| {
            let error = Error::with_range(
                s.span(),
                t.span(),
                "cannot assign here. Maybe you meant '==' instead of '='?",
            );
            input.report_error(error);
            Rc::new(Expression::Invalid(s.span().till(&t)))
        }))
        .parse(input);
    save_result(input, invalid_named_expression, &result);
    result
}

// invalid_assignment:
//     | a=invalid_ann_assign_target ':' expression {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(
//             a,
//             "only single target (not %s) can be annotated",
//             _PyPegen_get_expr_name(a)
//         )}
//     | a=star_named_expression ',' star_named_expressions* ':' expression {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "only single target (not tuple) can be annotated") }
//     | a=expression ':' expression {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "illegal target for annotation") }
//     | (star_targets '=')* a=star_expressions '=' {
//         RAISE_SYNTAX_ERROR_INVALID_TARGET(STAR_TARGETS, a) }
//     | (star_targets '=')* a=yield_expr '=' { RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "assignment to yield expression not possible") }
//     | a=star_expressions augassign (yield_expr | star_expressions) {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(
//             a,
//             "'%s' is an illegal expression for augmented assignment",
//             _PyPegen_get_expr_name(a)
//         )}
fn invalid_assignment(input: ParserInput) -> ParseResult<Statement> {
    left(invalid_ann_assign_target, pair(tok(TT::COLON), expression))
        .map(move |a| {
            let error = Error::with_underline(a.span(), "only single targets can be annotated");
            input.report_error(error);
            Statement::Invalid
        })
        .or(left(
            star_named_expression,
            pair(
                pair(tok(TT::COMMA), zero_or_more(star_named_expressions)),
                pair(tok(TT::COLON), expression),
            ),
        )
        .map(move |a| {
            let error =
                Error::with_underline(a.span(), "only single target (not tuple) can be annotated");
            input.report_error(error);
            Statement::Invalid
        }))
        .or(
            left(expression, pair(tok(TT::COLON), expression)).map(move |a| {
                let error = Error::with_underline(a.span(), "illegal target for annotation");
                input.report_error(error);
                Statement::Invalid
            }),
        )
        .or(right(
            zero_or_more(pair(star_targets, tok(TT::EQUAL))),
            left(star_expressions, tok(TT::EQUAL)),
        )
        .map(move |a| {
            let error = Error::with_underline(a.span(), "invalid target"); // FIXME: Message
            input.report_error(error);
            Statement::Invalid
        }))
        .or(right(
            zero_or_more(pair(star_targets, tok(TT::EQUAL))),
            left(yield_expr, tok(TT::EQUAL)),
        )
        .map(move |a| {
            let error =
                Error::with_underline(a.span(), "assignment to yield expression not possible");
            input.report_error(error);
            Statement::Invalid
        }))
        .or(left(
            star_expressions,
            pair(
                augassign,
                yield_expr.discard().or(star_expressions.discard()),
            ),
        )
        .map(move |a| {
            let error = Error::with_underline(
                a.span(),
                "this is an illegal expression for augmented assignment",
            );
            input.report_error(error);
            Statement::Invalid
        }))
        .parse(input)
}

// invalid_ann_assign_target[expr_ty]:
//     | list
//     | tuple
//     | '(' a=invalid_ann_assign_target ')' { a }
fn invalid_ann_assign_target(input: ParserInput) -> ParseResult<Rc<Expression>> {
    list.or(tuple)
        .or(left(
            right(tok(TT::LPAR), invalid_ann_assign_target),
            tok(TT::RPAR),
        ))
        .parse(input)
}

// invalid_del_stmt:
//     | 'del' a=star_expressions {
//         RAISE_SYNTAX_ERROR_INVALID_TARGET(DEL_TARGETS, a) }
fn invalid_del_stmt(input: ParserInput) -> ParseResult<Statement> {
    right(token(TT::KEYWORD, "del"), star_expressions)
        .map(move |a| {
            let error = Error::with_underline(
                a.span(),
                "invalid del target", // FIXME: Message
            );
            input.report_error(error);
            Statement::Invalid
        })
        .parse(input)
}

// invalid_block:
//     | NEWLINE !INDENT { RAISE_INDENTATION_ERROR("expected an indented block") }
fn invalid_block(input: ParserInput) -> ParseResult<Vec<Statement>> {
    left(tok(TT::NEWLINE), not(tok(TT::INDENT)))
        .map(move |t| {
            let error = Error::starting_from(t.span, "expected an indented block");
            input.report_error(error);
            vec![Statement::Invalid]
        })
        .parse(input)
}

// invalid_comprehension:
//     | ('[' | '(' | '{') a=starred_expression for_if_clauses {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "iterable unpacking cannot be used in comprehension") }
//     | ('[' | '{') a=star_named_expression ',' b=star_named_expressions for_if_clauses {
//         RAISE_SYNTAX_ERROR_KNOWN_RANGE(a, PyPegen_last_item(b, expr_ty),
//         "did you forget parentheses around the comprehension target?") }
//     | ('[' | '{') a=star_named_expression b=',' for_if_clauses {
//         RAISE_SYNTAX_ERROR_KNOWN_RANGE(a, b, "did you forget parentheses around the comprehension target?") }
fn invalid_comprehension(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(
        right(
            tok(TT::LSQB).or(tok(TT::LPAR)).or(tok(TT::LBRACE)),
            starred_expression,
        ),
        for_if_clauses,
    )
    .map(move |a| {
        let error = Error::starting_from(
            a.span(),
            "iterable unpacking cannot be used in comprehension",
        );
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    })
    .or(pair(
        right(tok(TT::LSQB).or(tok(TT::LBRACE)), star_named_expression),
        left(star_named_expressions, for_if_clauses),
    )
    .map(move |(a, b)| {
        let error = Error::with_range(
            a.span(),
            b.span(),
            "did you forget parentheses around the comprehension target?",
        );
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span().till(&b)))
    }))
    .or(pair(
        right(tok(TT::LSQB).or(tok(TT::LBRACE)), star_named_expression),
        left(tok(TT::COMMA), for_if_clauses),
    )
    .map(move |(a, b)| {
        let error = Error::with_range(
            a.span(),
            b.span(),
            "did you forget parentheses around the comprehension target?",
        );
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span().till(&b)))
    }))
    .parse(input)
}

// invalid_dict_comprehension:
//     | '{' a='**' bitwise_or for_if_clauses '}' {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "dict unpacking cannot be used in dict comprehension") }
fn invalid_dict_comprehension(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(
        right(
            tok(TT::LBRACE),
            right(tok(TT::DOUBLESTAR), left(bitwise_or, for_if_clauses)),
        ),
        tok(TT::RBRACE),
    )
    .map(move |a| {
        let error = Error::with_underline(
            a.span(),
            "dict unpacking cannot be used in dict comprehension",
        );
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    })
    .parse(input)
}

// invalid_parameters:
//     | a="/" ',' {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "at least one argument must precede /") }
//     | (slash_no_default | slash_with_default) param_maybe_default* a='/' {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "/ may appear only once") }
//     | slash_no_default? param_no_default* invalid_parameters_helper a=param_no_default {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "parameter without a default follows parameter with a default") }
//     | param_no_default* a='(' param_no_default+ ','? b=')' {
//         RAISE_SYNTAX_ERROR_KNOWN_RANGE(a, b, "Function parameters cannot be parenthesized") }
//     | (slash_no_default | slash_with_default)? param_maybe_default* '*' (',' | param_no_default) param_maybe_default* a='/' {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "/ must be ahead of *") }
//     | param_maybe_default+ '/' a='*' {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "expected comma between / and *") }
fn invalid_parameters(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(tok(TT::SLASH), tok(TT::COMMA))
        .map(move |a| {
            let error = Error::with_underline(a.span(), "at least one argument must precede /");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        })
        .or(right(
            pair(
                slash_no_default.or(slash_with_default),
                zero_or_more(param_maybe_default),
            ),
            tok(TT::SLASH),
        )
        .map(move |a| {
            let error = Error::with_underline(a.span(), "/ may appear only once");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }))
        .or(right(
            pair(
                pair(maybe(slash_no_default), zero_or_more(param_no_default)),
                slash_with_default
                    .discard()
                    .or(one_or_more(param_with_default).discard()),
            ),
            param_no_default,
        )
        .map(move |a| {
            let error = Error::with_underline(
                a.span(),
                "parameter without a default follows parameter with a default",
            );
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }))
        .or(right(
            zero_or_more(param_no_default),
            pair(
                tok(TT::LPAR),
                right(
                    pair(one_or_more(param_no_default), maybe(tok(TT::COMMA))),
                    tok(TT::RPAR),
                ),
            ),
        )
        .map(move |(a, b)| {
            let error = Error::with_range(
                a.span(),
                b.span(),
                "Function parameters cannot be parenthesized",
            );
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }))
        .or(right(
            maybe(slash_no_default.or(slash_with_default)),
            right(
                zero_or_more(param_maybe_default),
                right(
                    tok(TT::STAR),
                    right(
                        tok(TT::COMMA).discard().or(param_no_default.discard()),
                        right(zero_or_more(param_maybe_default), tok(TT::SLASH)),
                    ),
                ),
            ),
        )
        .map(move |a| {
            let error = Error::with_underline(a.span(), "/ must be ahead of *");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }))
        .or(right(
            one_or_more(param_maybe_default),
            right(tok(TT::SLASH), tok(TT::STAR)),
        )
        .map(move |a| {
            let error = Error::with_underline(a.span(), "expected comma between / and *");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }))
        .parse(input)
}

// invalid_default:
//     | a='=' &(')'|',') { RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "expected default value expression") }
fn invalid_default(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(tok(TT::EQUAL), lookahead(tok(TT::RPAR).or(tok(TT::COMMA))))
        .map(move |a| {
            let error = Error::with_underline(a.span(), "expected default value expression");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        })
        .parse(input)
}

// invalid_star_etc:
//     | a='*' (')' | ',' (')' | '**')) { RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "named arguments must follow bare *") }
//     | '*' ',' TYPE_COMMENT { RAISE_SYNTAX_ERROR("bare * has associated type comment") }
//     | '*' param a='=' { RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "var-positional argument cannot have default value") }
//     | '*' (param_no_default | ',') param_maybe_default* a='*' (param_no_default | ',') {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "* argument may appear only once") }
fn invalid_star_etc(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(
        tok(TT::STAR),
        tok(TT::RPAR)
            .discard()
            .or(pair(tok(TT::COMMA), tok(TT::RPAR).or(tok(TT::DOUBLESTAR))).discard()),
    )
    .map(move |a| {
        let error = Error::with_underline(a.span(), "expected default value expression");
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    })
    .or(
        left(tok(TT::STAR), pair(tok(TT::COMMA), tok(TT::TYPE_COMMENT))).map(move |a| {
            let error = Error::with_underline(a.span(), "bare * has associated type comment");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }),
    )
    .or(
        right(tok(TT::STAR), right(param, tok(TT::EQUAL))).map(move |a| {
            let error = Error::with_underline(
                a.span(),
                "var-positional argument cannot have default value",
            );
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }),
    )
    .or(right(
        tok(TT::STAR),
        right(
            pair(
                param_no_default.discard().or(tok(TT::COMMA).discard()),
                zero_or_more(param_maybe_default),
            ),
            left(
                tok(TT::STAR),
                param_no_default.discard().or(tok(TT::COMMA).discard()),
            ),
        ),
    )
    .map(move |a| {
        let error = Error::with_underline(a.span(), "* argument may appear only once");
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    }))
    .parse(input)
}

// invalid_kwds:
//     | '**' param a='=' { RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "var-keyword argument cannot have default value") }
//     | '**' param ',' a=param { RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "arguments cannot follow var-keyword argument") }
//     | '**' param ',' a[Token*]=('*'|'**'|'/') { RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "arguments cannot follow var-keyword argument") }
fn invalid_kwds(input: ParserInput) -> ParseResult<Rc<Expression>> {
    right(pair(tok(TT::DOUBLESTAR), param), tok(TT::EQUAL))
        .map(move |a| {
            let error =
                Error::with_underline(a.span(), "var-keyword argument cannot have default value");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        })
        .or(right(
            pair(tok(TT::DOUBLESTAR), param),
            right(tok(TT::COMMA), param),
        )
        .map(move |a| {
            let error =
                Error::with_underline(a.span(), "arguments cannot follow var-keyword argument");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }))
        .or(right(
            pair(tok(TT::DOUBLESTAR), param),
            right(
                tok(TT::COMMA),
                tok(TT::STAR).or(tok(TT::DOUBLESTAR)).or(tok(TT::SLASH)),
            ),
        )
        .map(move |a| {
            let error =
                Error::with_underline(a.span(), "arguments cannot follow var-keyword argument");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }))
        .parse(input)
}

// invalid_lambda_parameters_helper:
//     | a=lambda_slash_with_default { _PyPegen_singleton_seq(p, a) }
//     | lambda_param_with_default+
// invalid_lambda_parameters:
//     | a="/" ',' {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "at least one argument must precede /") }
//     | (lambda_slash_no_default | lambda_slash_with_default) lambda_param_maybe_default* a='/' {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "/ may appear only once") }
//     | lambda_slash_no_default? lambda_param_no_default* invalid_lambda_parameters_helper a=lambda_param_no_default {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "parameter without a default follows parameter with a default") }
//     | lambda_param_no_default* a='(' ','.lambda_param+ ','? b=')' {
//         RAISE_SYNTAX_ERROR_KNOWN_RANGE(a, b, "Lambda expression parameters cannot be parenthesized") }
//     | (lambda_slash_no_default | lambda_slash_with_default)? lambda_param_maybe_default* '*' (',' | lambda_param_no_default) lambda_param_maybe_default* a='/' {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "/ must be ahead of *") }
//     | lambda_param_maybe_default+ '/' a='*' {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "expected comma between / and *") }
fn invalid_lambda_parameters(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(tok(TT::SLASH), tok(TT::COMMA))
        .map(move |a| {
            let error = Error::with_underline(a.span(), "at least one argument must precede /");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        })
        .or(right(
            lambda_slash_no_default.or(lambda_slash_with_default),
            right(zero_or_more(lambda_param_maybe_default), tok(TT::SLASH)),
        )
        .map(move |a| {
            let error = Error::with_underline(a.span(), "/ may appear only once");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }))
        .or(right(
            maybe(lambda_slash_no_default),
            right(
                zero_or_more(lambda_param_no_default),
                right(
                    lambda_slash_with_default
                        .discard()
                        .or(one_or_more(lambda_param_with_default).discard()),
                    lambda_param_no_default,
                ),
            ),
        )
        .map(move |a| {
            let error = Error::with_underline(
                a.span(),
                "parameter without a default follows parameter with a default",
            );
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }))
        .or(right(
            zero_or_more(lambda_param_no_default),
            pair(
                tok(TT::STAR),
                right(
                    pair(sep_by(lambda_param, TT::COMMA), maybe(tok(TT::COMMA))),
                    tok(TT::RPAR),
                ),
            ),
        )
        .map(move |(a, b)| {
            let error = Error::with_range(
                a.span(),
                b.span(),
                "Lambda expression parameters cannot be parenthesized",
            );
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }))
        .or(right(
            pair(
                maybe(lambda_slash_no_default.or(lambda_slash_with_default)),
                zero_or_more(lambda_param_maybe_default),
            ),
            right(
                pair(
                    tok(TT::STAR),
                    pair(
                        tok(TT::COMMA)
                            .discard()
                            .or(lambda_param_no_default.discard()),
                        zero_or_more(lambda_param_maybe_default),
                    ),
                ),
                tok(TT::SLASH),
            ),
        )
        .map(move |a| {
            let error = Error::with_underline(a.span(), "/ must be ahead of *");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }))
        .or(right(
            pair(one_or_more(lambda_param_maybe_default), tok(TT::SLASH)),
            tok(TT::STAR),
        )
        .map(move |a| {
            let error = Error::with_underline(a.span(), "expected comma between / and *");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }))
        .parse(input)
}

// invalid_lambda_star_etc:
//     | '*' (':' | ',' (':' | '**')) { RAISE_SYNTAX_ERROR("named arguments must follow bare *") }
//     | '*' lambda_param a='=' { RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "var-positional argument cannot have default value") }
//     | '*' (lambda_param_no_default | ',') lambda_param_maybe_default* a='*' (lambda_param_no_default | ',') {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "* argument may appear only once") }
fn invalid_lambda_star_etc(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(
        tok(TT::STAR),
        tok(TT::COLON)
            .discard()
            .or(pair(tok(TT::COMMA), tok(TT::COLON).or(tok(TT::DOUBLESTAR))).discard()),
    )
    .map(move |a| {
        let error = Error::with_underline(a.span(), "named arguments must follow bare *");
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    })
    .or(
        right(pair(tok(TT::STAR), lambda_param), tok(TT::EQUAL)).map(move |a| {
            let error = Error::with_underline(
                a.span(),
                "var-positional argument cannot have default value",
            );
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }),
    )
    .or(left(
        right(
            tok(TT::STAR),
            right(
                lambda_param_no_default
                    .discard()
                    .or(tok(TT::COMMA).discard()),
                right(zero_or_more(lambda_param_maybe_default), tok(TT::STAR)),
            ),
        ),
        lambda_param_no_default
            .discard()
            .or(tok(TT::COMMA).discard()),
    )
    .map(move |a| {
        let error = Error::with_underline(a.span(), "* argument may appear only once");
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    }))
    .parse(input)
}

// invalid_lambda_kwds:
//     | '**' lambda_param a='=' { RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "var-keyword argument cannot have default value") }
//     | '**' lambda_param ',' a=lambda_param { RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "arguments cannot follow var-keyword argument") }
//     | '**' lambda_param ',' a[Token*]=('*'|'**'|'/') { RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "arguments cannot follow var-keyword argument") }
fn invalid_lambda_kwds(input: ParserInput) -> ParseResult<Rc<Expression>> {
    right(pair(tok(TT::DOUBLESTAR), lambda_param), tok(TT::EQUAL))
        .map(move |a| {
            let error =
                Error::with_underline(a.span(), "var-keyword argument cannot have default value");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        })
        .parse(input)
}

// invalid_double_type_comments:
//     | TYPE_COMMENT NEWLINE TYPE_COMMENT NEWLINE INDENT {
//         RAISE_SYNTAX_ERROR("Cannot have two type comments on def") }
fn invalid_double_type_comments(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(
        left(
            left(tok(TT::TYPE_COMMENT), tok(TT::NEWLINE)),
            pair(tok(TT::TYPE_COMMENT), tok(TT::NEWLINE)),
        ),
        tok(TT::INDENT),
    )
    .map(move |a| {
        let error = Error::starting_from(a.span(), "Cannot have two type comments on def");
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    })
    .parse(input)
}

// invalid_with_item:
//     | expression 'as' a=expression &(',' | ')' | ':') {
//         RAISE_SYNTAX_ERROR_INVALID_TARGET(STAR_TARGETS, a) }
// FIXME: message
fn invalid_with_item(input: ParserInput) -> ParseResult<Rc<Expression>> {
    right(
        pair(expression, token(TT::KEYWORD, "as")),
        left(
            expression,
            lookahead(tok(TT::COMMA).or(tok(TT::RPAR)).or(tok(TT::COLON))),
        ),
    )
    .map(move |a| {
        let error = Error::with_underline(a.span(), "Invalid target");
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    })
    .parse(input)
}

// invalid_for_target:
//     | 'async'? 'for' a=star_expressions {
//         RAISE_SYNTAX_ERROR_INVALID_TARGET(FOR_TARGETS, a) }
fn invalid_for_target(input: ParserInput) -> ParseResult<Statement> {
    right(
        pair(
            maybe(token(TT::KEYWORD, "async")),
            token_nodiscard(TT::KEYWORD, "for"),
        ),
        star_expressions,
    )
    .map(move |t| {
        let error = Error::starting_from(t.span(), "invalid for-targets"); // FIXME: appropriate message
        input.report_error(error);
        Statement::Invalid
    })
    .parse(input)
}

// invalid_group:
//     | '(' a=starred_expression ')' {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "cannot use starred expression here") }
//     | '(' a='**' expression ')' {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "cannot use double starred expression here") }
fn invalid_group(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(right(tok(TT::LPAR), starred_expression), tok(TT::RPAR))
        .map(move |a| {
            let error = Error::with_underline(a.span(), "cannot use starred expression here");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        })
        .or(left(
            right(
                tok(TT::LPAR),
                right(tok(TT::DOUBLESTAR), starred_expression),
            ),
            tok(TT::RPAR),
        )
        .map(move |a| {
            let error =
                Error::with_underline(a.span(), "cannot use double starred expression here");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }))
        .parse(input)
}

// invalid_import:
//     | a='import' ','.dotted_name+ 'from' dotted_name {
//         RAISE_SYNTAX_ERROR_STARTING_FROM(a, "Did you mean to use 'from ... import ...' instead?") }
fn invalid_import(input: ParserInput) -> ParseResult<Statement> {
    left(
        token_nodiscard(TT::KEYWORD, "import"),
        pair(
            pair(sep_by(dotted_name, TT::COMMA), token(TT::KEYWORD, "from")),
            dotted_name,
        ),
    )
    .map(move |a| {
        let error = Error::starting_from(
            a.span(),
            "Did you mean to use 'from ... import ...' instead?",
        );
        input.report_error(error);
        Statement::Invalid
    })
    .parse(input)
}

// invalid_import_from_targets:
//     | import_from_as_names ',' NEWLINE {
//         RAISE_SYNTAX_ERROR("trailing comma not allowed without surrounding parentheses") }
fn invalid_import_from_targets(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(import_from_as_names, pair(tok(TT::COMMA), tok(TT::NEWLINE)))
        .map(move |a| {
            let error = Error::with_underline(
                a.span(),
                "trailing comma not allowed without surrounding parentheses",
            );
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        })
        .parse(input)
}

// invalid_compound_stmt:
//     | a='elif' named_expression ':' { RAISE_SYNTAX_ERROR_STARTING_FROM(a, "'elif' must match an if-statement here") }
//     | a='else' ':' { RAISE_SYNTAX_ERROR_STARTING_FROM(a, "'else' must match a valid statement here") }
fn invalid_compound_stmt(input: ParserInput) -> ParseResult<Statement> {
    left(
        token_nodiscard(TT::KEYWORD, "elif"),
        pair(named_expression, tok(TT::COLON)),
    )
    .map(move |t| {
        let error = Error::starting_from(t.span, "'elif' must match an if-statement here");
        input.report_error(error);
        Statement::Invalid
    })
    .or(
        left(token_nodiscard(TT::KEYWORD, "else"), tok(TT::COLON)).map(move |t| {
            let error = Error::starting_from(t.span, "'else' must match a valid statement here");
            input.report_error(error);
            Statement::Invalid
        }),
    )
    .parse(input)
}

// invalid_with_stmt:
//     | ['async'] 'with' ','.(expression ['as' star_target])+ NEWLINE { RAISE_SYNTAX_ERROR("expected ':'") }
//     | ['async'] 'with' '(' ','.(expressions ['as' star_target])+ ','? ')' NEWLINE { RAISE_SYNTAX_ERROR("expected ':'") }
fn invalid_with_stmt(input: ParserInput) -> ParseResult<Statement> {
    right(
        pair(
            maybe(token(TT::KEYWORD, "async")),
            token(TT::KEYWORD, "with"),
        ),
        right(
            sep_by(
                pair(
                    expression,
                    maybe(pair(token(TT::KEYWORD, "as"), star_target)),
                ),
                TT::COMMA,
            )
            .or(left(
                right(
                    tok(TT::LPAR),
                    sep_by(
                        pair(
                            expression,
                            maybe(pair(token(TT::KEYWORD, "as"), star_target)),
                        ),
                        TT::COMMA,
                    ),
                ),
                pair(maybe(tok(TT::COMMA)), tok(TT::RPAR)),
            )),
            tok(TT::NEWLINE),
        ),
    )
    .map(move |t| {
        let error = Error::starting_from(t.span, "expected ':'");
        input.report_error(error);
        Statement::Invalid
    })
    .parse(input)
}

// invalid_with_stmt_indent:
//     | ['async'] a='with' ','.(expression ['as' star_target])+ ':' NEWLINE !INDENT {
//         RAISE_INDENTATION_ERROR("expected an indented block after 'with' statement on line %d", a->lineno) }
//     | ['async'] a='with' '(' ','.(expressions ['as' star_target])+ ','? ')' ':' NEWLINE !INDENT {
//         RAISE_INDENTATION_ERROR("expected an indented block after 'with' statement on line %d", a->lineno) }
fn invalid_with_stmt_indent(input: ParserInput) -> ParseResult<Statement> {
    right(
        pair(
            maybe(token(TT::KEYWORD, "async")),
            token(TT::KEYWORD, "with"),
        ),
        right(
            sep_by(
                pair(
                    expression,
                    maybe(pair(token(TT::KEYWORD, "as"), star_target)),
                ),
                TT::COMMA,
            )
            .or(left(
                right(
                    tok(TT::LPAR),
                    sep_by(
                        pair(
                            expression,
                            maybe(pair(token(TT::KEYWORD, "as"), star_target)),
                        ),
                        TT::COMMA,
                    ),
                ),
                pair(maybe(tok(TT::COMMA)), tok(TT::RPAR)),
            )),
            left(left(tok(TT::COLON), tok(TT::NEWLINE)), not(tok(TT::INDENT))),
        ),
    )
    .map(move |t| {
        let error = Error::with_line(
            t.span,
            "expected an indented block after 'with' statement on line",
        );
        input.report_error(error);
        Statement::Invalid
    })
    .parse(input)
}

// invalid_try_stmt:
//     | a='try' ':' NEWLINE !INDENT {
//         RAISE_INDENTATION_ERROR("expected an indented block after 'try' statement on line %d", a->lineno) }
//     | 'try' ':' block !('except' | 'finally') { RAISE_SYNTAX_ERROR("expected 'except' or 'finally' block") }
//     | 'try' ':' block* except_block+ a='except' b='*' expression ['as' NAME] ':' {
//         RAISE_SYNTAX_ERROR_KNOWN_RANGE(a, b, "cannot have both 'except' and 'except*' on the same 'try'") }
//     | 'try' ':' block* except_star_block+ a='except' [expression ['as' NAME]] ':' {
//         RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "cannot have both 'except' and 'except*' on the same 'try'") }
fn invalid_try_stmt(input: ParserInput) -> ParseResult<Statement> {
    left(
        left(token_nodiscard(TT::KEYWORD, "try"), tok(TT::COLON)),
        pair(tok(TT::NEWLINE), not(tok(TT::INDENT))),
    )
    .map(move |a| {
        let error = Error::with_line(
            a.span(),
            "expected an indented block after 'try' statement on line",
        );
        input.report_error(error);
        Statement::Invalid
    })
    .or(left(
        left(token_nodiscard(TT::KEYWORD, "try"), tok(TT::COLON)),
        pair(
            block,
            not(token(TT::KEYWORD, "except").or(token(TT::KEYWORD, "finally"))),
        ),
    )
    .map(move |a| {
        let error = Error::with_line(
            a.span(),
            "expected 'except' or 'finally' block in try statement beginning on line",
        );
        input.report_error(error);
        Statement::Invalid
    }))
    .or(right(
        left(token(TT::KEYWORD, "try"), tok(TT::COLON)),
        right(
            pair(zero_or_more(block), one_or_more(except_block)),
            left(
                pair(token_nodiscard(TT::KEYWORD, "except"), tok(TT::STAR)),
                pair(
                    maybe(pair(
                        expression,
                        maybe(pair(token(TT::KEYWORD, "as"), name)),
                    )),
                    tok(TT::COLON),
                ),
            ),
        )
        .map(move |(a, b)| {
            let error = Error::with_range(
                a.span(),
                b.span(),
                "cannot have both 'except' and 'except*' on the same 'try'",
            );
            input.report_error(error);
            Statement::Invalid
        }),
    ))
    .or(right(
        right(token(TT::KEYWORD, "try"), tok(TT::COLON)),
        right(
            right(zero_or_more(block), one_or_more(except_star_block)),
            left(
                token_nodiscard(TT::KEYWORD, "except"),
                pair(
                    maybe(pair(
                        expression,
                        maybe(pair(token(TT::KEYWORD, "as"), name)),
                    )),
                    tok(TT::COLON),
                ),
            ),
        ),
    )
    .map(move |a| {
        let error = Error::starting_from(
            a.span(),
            "cannot have both 'except' and 'except*' on the same 'try'",
        );
        input.report_error(error);
        Statement::Invalid
    }))
    .parse(input)
}

// invalid_except_stmt:
//     | 'except' '*'? a=expression ',' expressions ['as' NAME ] ':' {
//         RAISE_SYNTAX_ERROR_STARTING_FROM(a, "multiple exception types must be parenthesized") }
//     | a='except' '*'? expression ['as' NAME ] NEWLINE { RAISE_SYNTAX_ERROR("expected ':'") }
//     | a='except' NEWLINE { RAISE_SYNTAX_ERROR("expected ':'") }
//     | a='except' '*' (NEWLINE | ':') { RAISE_SYNTAX_ERROR("expected one or more exception types") }
fn invalid_except_stmt(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(
        right(
            pair(token(TT::KEYWORD, "except"), maybe(tok(TT::STAR))),
            expression,
        ),
        pair(
            tok(TT::COMMA),
            pair(
                expressions,
                pair(maybe(pair(token(TT::KEYWORD, "as"), name)), tok(TT::COLON)),
            ),
        ),
    )
    .map(move |a| {
        let error =
            Error::starting_from(a.span(), "multiple exception types must be parenthesized");
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    })
    .or(left(
        left(token_nodiscard(TT::KEYWORD, "except"), maybe(tok(TT::STAR))),
        pair(
            expression,
            pair(
                maybe(pair(token(TT::KEYWORD, "as"), name)),
                tok(TT::NEWLINE),
            ),
        ),
    )
    .map(move |a| {
        let error = Error::starting_from(a.span(), "expected ':'");
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    }))
    .or(
        left(token_nodiscard(TT::KEYWORD, "except"), tok(TT::NEWLINE)).map(move |a| {
            let error = Error::starting_from(a.span(), "expected ':'");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }),
    )
    .or(left(
        token_nodiscard(TT::KEYWORD, "except"),
        pair(tok(TT::STAR), tok(TT::NEWLINE).or(tok(TT::STAR))),
    )
    .map(move |a| {
        let error = Error::starting_from(a.span(), "expected one or more exception types");
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    }))
    .parse(input)
}

// invalid_finally_stmt:
//     | a='finally' ':' NEWLINE !INDENT {
//         RAISE_INDENTATION_ERROR("expected an indented block after 'finally' statement on line %d", a->lineno) }
fn invalid_finally_stmt(input: ParserInput) -> ParseResult<Vec<Statement>> {
    left(
        left(
            left(token_nodiscard(TT::KEYWORD, "finally"), tok(TT::COLON)),
            tok(TT::NEWLINE),
        ),
        not(tok(TT::INDENT)),
    )
    .map(move |a| {
        let error = Error::with_line(
            a.span(),
            "expected an indented block after 'finally' statement on line ",
        );
        input.report_error(error);
        vec![Statement::Invalid]
    })
    .parse(input)
}

// invalid_except_stmt_indent:
//     | a='except' expression ['as' NAME ] ':' NEWLINE !INDENT {
//         RAISE_INDENTATION_ERROR("expected an indented block after 'except' statement on line %d", a->lineno) }
//     | a='except' ':' NEWLINE !INDENT { RAISE_INDENTATION_ERROR("expected an indented block after 'except' statement on line %d", a->lineno) }
fn invalid_except_stmt_indent(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(
        left(
            left(token_nodiscard(TT::KEYWORD, "except"), expression),
            maybe(pair(token(TT::KEYWORD, "as"), name)),
        ),
        pair(pair(tok(TT::COLON), tok(TT::NEWLINE)), not(tok(TT::INDENT))),
    )
    .map(move |a| {
        let error = Error::with_line(
            a.span(),
            "expected an indented block after 'except' statement on line ",
        );
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    })
    .or(left(
        token_nodiscard(TT::KEYWORD, "except"),
        pair(pair(tok(TT::COLON), tok(TT::NEWLINE)), not(tok(TT::INDENT))),
    )
    .map(move |a| {
        let error = Error::with_line(
            a.span(),
            "expected an indented block after 'except' statement on line ",
        );
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    }))
    .parse(input)
}

// invalid_except_star_stmt_indent:
//     | a='except' '*' expression ['as' NAME ] ':' NEWLINE !INDENT {
//         RAISE_INDENTATION_ERROR("expected an indented block after 'except*' statement on line %d", a->lineno) }
fn invalid_except_star_stmt_indent(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(
        left(
            left(
                token_nodiscard(TT::KEYWORD, "except"),
                pair(tok(TT::STAR), expression),
            ),
            maybe(pair(token(TT::KEYWORD, "as"), name)),
        ),
        pair(pair(tok(TT::COLON), tok(TT::NEWLINE)), not(tok(TT::INDENT))),
    )
    .map(move |a| {
        let error = Error::with_line(
            a.span(),
            "expected an indented block after 'except' statement on line ",
        );
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    })
    .parse(input)
}

// invalid_match_stmt:
//     | "match" subject_expr NEWLINE { CHECK_VERSION(void*, 10, "Pattern matching is", RAISE_SYNTAX_ERROR("expected ':'") ) }
//     | a="match" subject=subject_expr ':' NEWLINE !INDENT {
//         RAISE_INDENTATION_ERROR("expected an indented block after 'match' statement on line %d", a->lineno) }
fn invalid_match_stmt(input: ParserInput) -> ParseResult<Statement> {
    left(
        token_nodiscard(TT::NAME, "match"),
        pair(
            subject_expr,
            pair(tok(TT::COLON), pair(tok(TT::NEWLINE), not(tok(TT::INDENT)))),
        ),
    )
    .map(move |a| {
        let error = Error::with_line(
            a.span(),
            "expected an indented block after 'match' statement on line ",
        );
        input.report_error(error);
        Statement::Invalid
    })
    .parse(input)
}

// invalid_case_block:
//     | "case" patterns guard? NEWLINE { RAISE_SYNTAX_ERROR("expected ':'") }
//     | a="case" patterns guard? ':' NEWLINE !INDENT {
//         RAISE_INDENTATION_ERROR("expected an indented block after 'case' statement on line %d", a->lineno) }
fn invalid_case_block(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(
        left(
            left(token_nodiscard(TT::NAME, "case"), patterns),
            maybe(guard),
        ),
        tok(TT::NEWLINE),
    )
    .map(move |a| {
        let error = Error::starting_from(a.span(), "expected ':'");
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    })
    .or(left(
        left(
            left(token_nodiscard(TT::NAME, "case"), patterns),
            maybe(guard),
        ),
        pair(pair(tok(TT::COLON), tok(TT::NEWLINE)), not(tok(TT::INDENT))),
    )
    .map(move |a| {
        let error = Error::with_line(
            a.span(),
            "expected an indented block after 'case' statement on line ",
        );
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span()))
    }))
    .parse(input)
}

// invalid_as_pattern:
//     | or_pattern 'as' a="_" { RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "cannot use '_' as a target") }
//     | or_pattern 'as' !NAME a=expression { RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "invalid pattern target") }
fn invalid_as_pattern(input: ParserInput) -> ParseResult<Pattern> {
    right(
        pair(or_pattern, token(TT::KEYWORD, "as")),
        name.pred(|n| *n.name == *"_"),
    )
    .map(move |a| {
        let error = Error::starting_from(a.span(), "cannot use '_' as a target");
        input.report_error(error);
        Pattern::Invalid(a.span())
    })
    .or(right(
        pair(or_pattern, token(TT::KEYWORD, "as")),
        right(not(name), expression),
    )
    .map(move |a| {
        let error = Error::starting_from(a.span(), "invalid pattern target");
        input.report_error(error);
        Pattern::Invalid(a.span())
    }))
    .parse(input)
}

// invalid_class_pattern:
//     | name_or_attr '(' a=invalid_class_argument_pattern  { RAISE_SYNTAX_ERROR_KNOWN_RANGE(
//         PyPegen_first_item(a, pattern_ty),
//         PyPegen_last_item(a, pattern_ty),
//         "positional patterns follow keyword patterns") }
fn invalid_class_pattern(input: ParserInput) -> ParseResult<Pattern> {
    right(
        pair(name_or_attr, tok(TT::LPAR)),
        invalid_class_argument_pattern,
    )
    .map(move |a| {
        let error = Error::with_underline(a.span(), "positional patterns follow keyword patterns");
        input.report_error(error);
        Pattern::Invalid(a.span())
    })
    .parse(input)
}

// invalid_class_argument_pattern[asdl_pattern_seq*]:
//     | [positional_patterns ','] keyword_patterns ',' a=positional_patterns { a }
fn invalid_class_argument_pattern(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        maybe(left(positional_patterns, tok(TT::COMMA))),
        keyword_patterns,
    )
    .map(|(p, q)| {
        let span = match p {
            Some(p) if !p.is_empty() => p.first().unwrap().span().till(&q),
            _ => q.span(),
        };
        Rc::new(Expression::Invalid(span))
    })
    .parse(input)
}

fn missing_block_or_colon_on_keyword_named_expr<'a>(
    input: ParserInput<'a>,
    keyword: &'static str,
) -> BoxedParser<'a, Statement> {
    left(
        right(token(TT::KEYWORD, keyword), named_expression),
        tok(TT::NEWLINE),
    )
    .map(move |t| {
        let error = Error::starting_from(t.span(), "expected ':'");
        input.report_error(error);
        Statement::Invalid
    })
    .or(left(
        left(
            left(token_nodiscard(TT::KEYWORD, keyword), named_expression),
            pair(tok(TT::COLON), tok(TT::NEWLINE)),
        ),
        not(tok(TT::INDENT)),
    )
    .map(move |t| {
        let error = Error::with_line(
            t.span,
            &format!(
                "expected an indented block after '{}' statement on line ",
                keyword
            ),
        );
        input.report_error(error);
        Statement::Invalid
    }))
}

// invalid_if_stmt:
//     | 'if' named_expression NEWLINE { RAISE_SYNTAX_ERROR("expected ':'") }
//     | a='if' a=named_expression ':' NEWLINE !INDENT {
//         RAISE_INDENTATION_ERROR("expected an indented block after 'if' statement on line %d", a->lineno) }
fn invalid_if_stmt(input: ParserInput) -> ParseResult<Statement> {
    missing_block_or_colon_on_keyword_named_expr(input, "if").parse(input)
}

// invalid_elif_stmt:
//     | 'elif' named_expression NEWLINE { RAISE_SYNTAX_ERROR("expected ':'") }
//     | a='elif' named_expression ':' NEWLINE !INDENT {
//         RAISE_INDENTATION_ERROR("expected an indented block after 'elif' statement on line %d", a->lineno) }
fn invalid_elif_stmt(
    input: ParserInput,
) -> ParseResult<(
    Vec<(Rc<Expression>, Vec<Statement>)>,
    Option<Vec<Statement>>,
)> {
    missing_block_or_colon_on_keyword_named_expr(input, "elif")
        .map(|s| {
            (
                vec![(
                    Rc::new(Expression::Invalid(s.span())),
                    vec![Statement::Invalid],
                )],
                None,
            )
        })
        .parse(input)
}

// invalid_else_stmt:
//     | a='else' ':' NEWLINE !INDENT {
//         RAISE_INDENTATION_ERROR("expected an indented block after 'else' statement on line %d", a->lineno) }
fn invalid_else_stmt(input: ParserInput) -> ParseResult<Vec<Statement>> {
    missing_block_or_colon_on_keyword_named_expr(input, "else")
        .map(|s| vec![s])
        .parse(input)
}

// invalid_while_stmt:
//     | 'while' named_expression NEWLINE { RAISE_SYNTAX_ERROR("expected ':'") }
//     | a='while' named_expression ':' NEWLINE !INDENT {
//         RAISE_INDENTATION_ERROR("expected an indented block after 'while' statement on line %d", a->lineno) }
fn invalid_while_stmt(input: ParserInput) -> ParseResult<Statement> {
    missing_block_or_colon_on_keyword_named_expr(input, "while").parse(input)
}

// invalid_for_stmt:
//     | ['async'] 'for' star_targets 'in' star_expressions NEWLINE { RAISE_SYNTAX_ERROR("expected ':'") }
//     | ['async'] a='for' star_targets 'in' star_expressions ':' NEWLINE !INDENT {
//         RAISE_INDENTATION_ERROR("expected an indented block after 'for' statement on line %d", a->lineno) }
fn invalid_for_stmt(input: ParserInput) -> ParseResult<Statement> {
    left(
        left(
            left(
                right(
                    maybe(token(TT::KEYWORD, "async")),
                    token_nodiscard(TT::KEYWORD, "for"),
                ),
                star_targets,
            ),
            pair(token(TT::KEYWORD, "in"), star_expressions),
        ),
        tok(TT::NEWLINE),
    )
    .map(move |t| {
        let error = Error::starting_from(t.span, "expected ':'");
        input.report_error(error);
        Statement::Invalid
    })
    .or(left(
        left(
            left(
                left(
                    right(
                        maybe(token(TT::KEYWORD, "async")),
                        token_nodiscard(TT::KEYWORD, "for"),
                    ),
                    star_targets,
                ),
                pair(token(TT::KEYWORD, "in"), star_expressions),
            ),
            pair(tok(TT::COLON), tok(TT::NEWLINE)),
        ),
        not(tok(TT::INDENT)),
    )
    .map(move |t| {
        let error = Error::with_line(
            t.span,
            "expected an indented block after 'for' statement on line",
        );
        input.report_error(error);
        Statement::Invalid
    }))
    .parse(input)
}

// invalid_def_raw:
//     | ['async'] a='def' NAME [type_params] '(' [params] ')' ['->' expression] ':' NEWLINE !INDENT {
//         RAISE_INDENTATION_ERROR("expected an indented block after function definition on line %d", a->lineno) }
fn invalid_def_raw(input: ParserInput) -> ParseResult<Statement> {
    left(
        right(
            maybe(token(TT::KEYWORD, "async")),
            left(token_nodiscard(TT::KEYWORD, "def"), name),
        ),
        pair(
            pair(
                maybe(type_params),
                pair(tok(TT::LPAR), pair(maybe(params), tok(TT::RPAR))),
            ),
            pair(
                maybe(pair(tok(TT::RARROW), expression)),
                pair(pair(tok(TT::COLON), tok(TT::NEWLINE)), not(tok(TT::INDENT))),
            ),
        ),
    )
    .map(move |a| {
        let error = Error::with_line(
            a.span(),
            "expected an indented block after function definition on line ",
        );
        input.report_error(error);
        Statement::Invalid
    })
    .parse(input)
}

// invalid_class_def_raw:
//     | 'class' NAME [type_params] ['(' [arguments] ')'] NEWLINE { RAISE_SYNTAX_ERROR("expected ':'") }
//     | a='class' NAME [type_params] ['(' [arguments] ')'] ':' NEWLINE !INDENT {
//         RAISE_INDENTATION_ERROR("expected an indented block after class definition on line %d", a->lineno) }
fn invalid_class_def_raw(input: ParserInput) -> ParseResult<Statement> {
    left(
        left(
            left(
                token_nodiscard(TT::KEYWORD, "class"),
                pair(name, maybe(type_params)),
            ),
            maybe(right(tok(TT::LPAR), left(arguments, tok(TT::RPAR)))),
        ),
        tok(TT::NEWLINE),
    )
    .map(move |t| {
        let error = Error::starting_from(t.span, "expected ':'");
        input.report_error(error);
        Statement::Invalid
    })
    .or(left(
        left(
            left(
                left(
                    token_nodiscard(TT::KEYWORD, "class"),
                    pair(name, maybe(type_params)),
                ),
                maybe(right(tok(TT::LPAR), left(arguments, tok(TT::RPAR)))),
            ),
            pair(tok(TT::COLON), tok(TT::NEWLINE)),
        ),
        not(tok(TT::INDENT)),
    )
    .map(move |t| {
        let error = Error::with_line(
            t.span,
            "expected an indented block after 'class' definition on line",
        );
        input.report_error(error);
        Statement::Invalid
    }))
    .parse(input)
}

// invalid_double_starred_kvpairs:
//     | ','.double_starred_kvpair+ ',' invalid_kvpair
//     | expression ':' a='*' bitwise_or { RAISE_SYNTAX_ERROR_STARTING_FROM(a, "cannot use a starred expression in a dictionary value") }
//     | expression a=':' &('}'|',') { RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "expression expected after dictionary key and ':'") }
fn invalid_double_starred_kvpairs(input: ParserInput) -> ParseResult<Rc<Expression>> {
    right(
        sep_by(double_starred_kvpair, TT::COMMA),
        right(tok(TT::COMMA), invalid_kvpair),
    )
    .or(right(
        pair(expression, tok(TT::COLON)),
        left(tok(TT::STAR), bitwise_or),
    )
    .map(move |a| {
        let error = Error::starting_from(
            a.span(),
            "cannot use a starred expression in a dictionary value",
        );
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span))
    }))
    .or(left(
        right(expression, tok(TT::COLON)),
        lookahead(tok(TT::RBRACE).or(tok(TT::COMMA))),
    )
    .map(move |a| {
        let error =
            Error::starting_from(a.span(), "expression expected after dictionary key and ':'");
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span))
    }))
    .parse(input)
}

// invalid_kvpair:
//     | a=expression !(':') {
//         RAISE_ERROR_KNOWN_LOCATION(p, PyExc_SyntaxError, a->lineno, a->end_col_offset - 1, a->end_lineno, -1, "':' expected after dictionary key") }
//     | expression ':' a='*' bitwise_or { RAISE_SYNTAX_ERROR_STARTING_FROM(a, "cannot use a starred expression in a dictionary value") }
//     | expression a=':' &('}'|',') {RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "expression expected after dictionary key and ':'") }
fn invalid_kvpair(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(expression, not(tok(TT::COLON)))
        .map(move |a| {
            let error = Error::starting_from(a.span(), "':' expected after dictionary key");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        })
        .or(left(
            right(expression, tok(TT::COLON)),
            pair(tok(TT::STAR), bitwise_or),
        )
        .map(move |a| {
            let error = Error::starting_from(
                a.span(),
                "cannot use a starred expression in a dictionary value",
            );
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }))
        .or(left(
            right(expression, tok(TT::COLON)),
            lookahead(tok(TT::RBRACE).or(tok(TT::COMMA))),
        )
        .map(move |a| {
            let error =
                Error::starting_from(a.span(), "expression expected after dictionary key and ':'");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span))
        }))
        .parse(input)
}

// invalid_starred_expression:
//     | a='*' expression '=' b=expression { RAISE_SYNTAX_ERROR_KNOWN_RANGE(a, b, "cannot assign to iterable argument unpacking") }
fn invalid_starred_expression(input: ParserInput) -> ParseResult<Rc<Expression>> {
    pair(
        left(tok(TT::STAR), left(expression, tok(TT::EQUAL))),
        expression,
    )
    .map(move |(a, b)| {
        let error = Error::with_range(
            a.span(),
            b.span(),
            "cannot assign to iterable argument unpacking",
        );
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span().till(&b)))
    })
    .parse(input)
}

// invalid_replacement_field:
//     | '{' a='=' { RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "f-string: valid expression required before '='") }
//     | '{' a='!' { RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "f-string: valid expression required before '!'") }
//     | '{' a=':' { RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "f-string: valid expression required before ':'") }
//     | '{' a='}' { RAISE_SYNTAX_ERROR_KNOWN_LOCATION(a, "f-string: valid expression required before '}'") }
//     | '{' !(yield_expr | star_expressions) { RAISE_SYNTAX_ERROR_ON_NEXT_TOKEN("f-string: expecting a valid expression after '{'")}
//     | '{' (yield_expr | star_expressions) !('=' | '!' | ':' | '}') {
//         PyErr_Occurred() ? NULL : RAISE_SYNTAX_ERROR_ON_NEXT_TOKEN("f-string: expecting '=', or '!', or ':', or '}'") }
//     | '{' (yield_expr | star_expressions) '=' !('!' | ':' | '}') {
//         PyErr_Occurred() ? NULL : RAISE_SYNTAX_ERROR_ON_NEXT_TOKEN("f-string: expecting '!', or ':', or '}'") }
//     | '{' (yield_expr | star_expressions) '='? invalid_conversion_character
//     | '{' (yield_expr | star_expressions) '='? ['!' NAME] !(':' | '}') {
//         PyErr_Occurred() ? NULL : RAISE_SYNTAX_ERROR_ON_NEXT_TOKEN("f-string: expecting ':' or '}'") }
//     | '{' (yield_expr | star_expressions) '='? ['!' NAME] ':' fstring_format_spec* !'}' {
//         PyErr_Occurred() ? NULL : RAISE_SYNTAX_ERROR_ON_NEXT_TOKEN("f-string: expecting '}', or format specs") }
//     | '{' (yield_expr | star_expressions) '='? ['!' NAME] !'}' {
//         PyErr_Occurred() ? NULL : RAISE_SYNTAX_ERROR_ON_NEXT_TOKEN("f-string: expecting '}'") }
// FIXME: Raise error on next token
fn invalid_replacement_field(input: ParserInput) -> ParseResult<Rc<Expression>> {
    right(tok(TT::LBRACE), tok(TT::EQUAL))
        .map(move |a| {
            let error =
                Error::with_underline(a.span(), "f-string: valid expression required before '='");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span))
        })
        .or(right(tok(TT::LBRACE), tok(TT::EXCLAMATION)).map(move |a| {
            let error =
                Error::with_underline(a.span(), "f-string: valid expression required before '!'");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span))
        }))
        .or(right(tok(TT::LBRACE), tok(TT::COLON)).map(move |a| {
            let error =
                Error::with_underline(a.span(), "f-string: valid expression required before ':'");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span))
        }))
        .or(right(tok(TT::LBRACE), tok(TT::RBRACE)).map(move |a| {
            let error =
                Error::with_underline(a.span(), "f-string: valid expression required before '}'");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span))
        }))
        .or(left(
            tok(TT::LBRACE),
            not(yield_expr.discard().or(star_expressions.discard())),
        )
        .map(move |a| {
            let error =
                Error::starting_from(a.span(), "f-string: expecting a valid expression after '{'");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span))
        }))
        .or(left(
            right(
                tok(TT::LBRACE),
                yield_expr.map(|e| vec![e]).or(star_expressions),
            ),
            not(tok(TT::EQUAL)
                .or(tok(TT::EXCLAMATION))
                .or(tok(TT::COLON))
                .or(tok(TT::RBRACE))),
        )
        .map(move |a| {
            let error =
                Error::starting_from(a.span(), "f-string: expecting '=', or '!', or ':', or '}'");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }))
        .or(right(
            pair(
                tok(TT::LBRACE),
                yield_expr.map(|e| vec![e]).or(star_expressions),
            ),
            right(maybe(tok(TT::EQUAL)), invalid_conversion_character),
        ))
        .or(left(
            right(
                tok(TT::LBRACE),
                yield_expr.map(|e| vec![e]).or(star_expressions),
            ),
            pair(
                maybe(tok(TT::EQUAL)),
                pair(
                    maybe(pair(tok(TT::EXCLAMATION), name)),
                    not(tok(TT::COLON).or(tok(TT::RBRACE))),
                ),
            ),
        )
        .map(move |a| {
            let error = Error::starting_from(a.span(), "f-string: expecting ':', or '}'");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }))
        .or(left(
            right(
                tok(TT::LBRACE),
                yield_expr.map(|e| vec![e]).or(star_expressions),
            ),
            pair(
                maybe(tok(TT::EQUAL)),
                pair(
                    maybe(pair(tok(TT::EXCLAMATION), name)),
                    pair(
                        pair(tok(TT::COLON), zero_or_more(fstring_format_spec)),
                        not(tok(TT::RBRACE)),
                    ),
                ),
            ),
        )
        .map(move |a| {
            let error = Error::starting_from(a.span(), "f-string: expecting '}', or format specs");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }))
        .or(left(
            right(
                tok(TT::LBRACE),
                yield_expr.map(|e| vec![e]).or(star_expressions),
            ),
            pair(
                maybe(tok(TT::EQUAL)),
                pair(
                    maybe(pair(tok(TT::EXCLAMATION), name)),
                    not(tok(TT::RBRACE)),
                ),
            ),
        )
        .map(move |a| {
            let error = Error::starting_from(a.span(), "f-string: expecting '}'");
            input.report_error(error);
            Rc::new(Expression::Invalid(a.span()))
        }))
        .parse(input)
}

// invalid_conversion_character:
//     | '!' &(':' | '}') { RAISE_SYNTAX_ERROR_ON_NEXT_TOKEN("f-string: missing conversion character") }
//     | '!' !NAME { RAISE_SYNTAX_ERROR_ON_NEXT_TOKEN("f-string: invalid conversion character") }
fn invalid_conversion_character(input: ParserInput) -> ParseResult<Rc<Expression>> {
    left(
        tok(TT::EXCLAMATION),
        lookahead(tok(TT::COLON).or(tok(TT::RBRACE))),
    )
    .map(move |a| {
        let error = Error::starting_from(a.span(), "f-string: missing conversion character");
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span))
    })
    .or(left(tok(TT::EXCLAMATION), not(name)).map(move |a| {
        let error = Error::starting_from(a.span(), "f-string: invalid conversion character");
        input.report_error(error);
        Rc::new(Expression::Invalid(a.span))
    }))
    .parse(input)
}
