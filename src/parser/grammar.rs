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

use super::ast::*;
use super::combinators::*;
use super::tokenizer::{Token, TokenType as TT};

pub(crate) fn parse(input: &[Token]) -> ParseResult<Vec<Statement>> {
    file_.parse(input)
}

// # STARTING RULES
// # ==============
// file: [statements] ENDMARKER
fn file_(input: &[Token]) -> ParseResult<Vec<Statement>> {
    left(maybe(statements), tok(TT::ENDMARKER))
        .map(|v| v.unwrap_or(vec![]))
        .parse(input)
}

// interactive: statement_newline
fn interactive(input: &[Token]) -> ParseResult<Vec<Statement>> {
    statement_newline.parse(input)
}

// eval: expressions NEWLINE* ENDMARKER
fn eval(input: &[Token]) -> ParseResult<Vec<Expression>> {
    left(
        expressions,
        pair(zero_or_more(tok(TT::NEWLINE)), tok(TT::ENDMARKER)),
    )
    .parse(input)
}

// func_type: '(' [type_expressions] ')' '->' expression NEWLINE* ENDMARKER

// # GENERAL STATEMENTS
// # ==================

fn pass(input: &[Token]) -> ParseResult<Statement> {
    token(TT::KEYWORD, "pass")
        .map(|_| Statement::Pass)
        .parse(input)
}

fn break_(input: &[Token]) -> ParseResult<Statement> {
    token(TT::KEYWORD, "break")
        .map(|_| Statement::Break)
        .parse(input)
}

fn continue_(input: &[Token]) -> ParseResult<Statement> {
    token(TT::KEYWORD, "continue")
        .map(|_| Statement::Continue)
        .parse(input)
}

// statements: statement+
fn statements(input: &[Token]) -> ParseResult<Vec<Statement>> {
    one_or_more(statement).map(|s| s.concat()).parse(input)
}

// statement: compound_stmt  | simple_stmts
fn statement(input: &[Token]) -> ParseResult<Vec<Statement>> {
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
fn statement_newline(input: &[Token]) -> ParseResult<Vec<Statement>> {
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
fn simple_stmts(input: &[Token]) -> ParseResult<Vec<Statement>> {
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
fn simple_stmt(input: &[Token]) -> ParseResult<Statement> {
    assignment
        // .or(type_alias)
        .or(star_expressions.map(|v| Statement::Expressions(v)))
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
fn compound_stmt(input: &[Token]) -> ParseResult<Statement> {
    function_def
        .or(if_stmt)
        .or(class_def)
        .or(with_stmt)
        .or(for_stmt)
        .or(try_stmt)
        .or(while_stmt)
        // .or(match_stmt) TODO
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
// TODO: type comment, ~
fn assignment(input: &[Token]) -> ParseResult<Statement> {
    pair(
        pair(left(name, tok(TT::COLON)), expression),
        maybe(right(tok(TT::EQUAL), annotated_rhs)),
    )
    .map(|((n, t), r)| Statement::Assignment(vec![Expression::Name(n)], None, r, Some(Box::new(t))))
    .or(pair(
        pair(
            right(tok(TT::LPAR), left(single_target, tok(TT::RPAR)))
                .or(single_subscript_attribute_target),
            right(tok(TT::COLON), expression),
        ),
        maybe(right(tok(TT::EQUAL), annotated_rhs)),
    )
    .map(|((l, t), r)| Statement::Assignment(vec![l], None, r, Some(Box::new(t)))))
    .or(left(
        pair(
            one_or_more(left(star_targets, tok(TT::EQUAL))),
            yield_expr.map(|e| vec![e]).or(star_expressions),
        ),
        not(tok(TT::EQUAL)),
    )
    .map(|(l, r)| Statement::Assignment(l.concat(), None, Some(r), None)))
    .or(pair(
        pair(single_target, augassign),
        yield_expr.map(|e| vec![e]).or(star_expressions),
    )
    .map(|((l, o), r)| Statement::Assignment(vec![l], Some(o.into()), Some(r), None)))
    .parse(input)
}

// annotated_rhs: yield_expr | star_expressions
fn annotated_rhs(input: &[Token]) -> ParseResult<Vec<Expression>> {
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
fn augassign(input: &[Token]) -> ParseResult<Token> {
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
fn return_stmt(input: &[Token]) -> ParseResult<Statement> {
    right(token(TT::KEYWORD, "return"), star_expressions)
        .map(|v| Statement::Return(v))
        .parse(input)
}

// raise_stmt:
//     | 'raise' expression ['from' expression ]
//     | 'raise'
fn raise_stmt(input: &[Token]) -> ParseResult<Statement> {
    pair(
        right(token(TT::KEYWORD, "raise"), expression),
        maybe(right(token(TT::KEYWORD, "from"), expression)),
    )
    .map(|(e, f)| Statement::Raise(Some(Box::new(e)), f.map(|x| Box::new(x))))
    .or(token(TT::KEYWORD, "raise").map(|_| Statement::Raise(None, None)))
    .parse(input)
}

// global_stmt: 'global' ','.NAME+
fn global_stmt(input: &[Token]) -> ParseResult<Statement> {
    right(token(TT::KEYWORD, "global"), sep_by(name, TT::COMMA))
        .map(|n| Statement::Global(n))
        .parse(input)
}

// nonlocal_stmt: 'nonlocal' ','.NAME+
fn nonlocal_stmt(input: &[Token]) -> ParseResult<Statement> {
    right(token(TT::KEYWORD, "nonlocal"), sep_by(name, TT::COMMA))
        .map(|n| Statement::Nonlocal(n))
        .parse(input)
}

// del_stmt:
//     | 'del' del_targets &(';' | NEWLINE)
fn del_stmt(input: &[Token]) -> ParseResult<Statement> {
    left(
        right(token(TT::KEYWORD, "del"), del_targets),
        lookahead(tok(TT::SEMI).or(tok(TT::NEWLINE))),
    )
    .map(|t| Statement::Del(Box::new(t)))
    .parse(input)
}

// yield_stmt: yield_expr
fn yield_stmt(input: &[Token]) -> ParseResult<Statement> {
    yield_expr
        .map(|e| Statement::Yield(Box::new(e)))
        .parse(input)
}

// assert_stmt: 'assert' expression [',' expression ]
fn assert_stmt(input: &[Token]) -> ParseResult<Statement> {
    right(
        token(TT::KEYWORD, "assert"),
        pair(expression, maybe(right(tok(TT::COMMA), expression))),
    )
    .map(|(e, m)| Statement::Assert(Box::new(e), m.map(|e| Box::new(e))))
    .parse(input)
}

// import_stmt:
//     | import_name
//     | import_from
fn import_stmt(input: &[Token]) -> ParseResult<Statement> {
    import_name
        .or(import_from.map(|t| vec![t]))
        .map(|v| Statement::Import(v))
        .parse(input)
}

// # Import statements
// # -----------------

// import_name: 'import' dotted_as_names
fn import_name(input: &[Token]) -> ParseResult<Vec<Import>> {
    right(token(TT::KEYWORD, "import"), dotted_as_names)
        .map(|ms| {
            ms.iter()
                .map(|m| Import {
                    module: m.clone(),
                    items: vec![],
                })
                .collect()
        })
        .parse(input)
}

fn import_level(input: &[Token]) -> ParseResult<usize> {
    zero_or_more(tok(TT::DOT).or(tok(TT::ELLIPSIS)))
        .map(count_dots)
        .parse(input)
}

fn import_level_at_least_one(input: &[Token]) -> ParseResult<usize> {
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
fn import_from(input: &[Token]) -> ParseResult<Import> {
    pair(
        right(token(TT::KEYWORD, "from"), pair(import_level, dotted_name)),
        right(token(TT::KEYWORD, "import"), import_from_targets),
    )
    .or(pair(
        right(token(TT::KEYWORD, "from"), import_level_at_least_one),
        right(token(TT::KEYWORD, "import"), import_from_targets),
    )
    .map(|(n, t)| ((n, vec![]), t)))
    .map(|((rel_level, path), items)| Import {
        module: Module { rel_level, path },
        items,
    })
    .parse(input)
}

// import_from_targets:
//     | '(' import_from_as_names [','] ')'
//     | import_from_as_names !','
//     | '*'
fn import_from_targets(input: &[Token]) -> ParseResult<Vec<ImportItem>> {
    left(
        right(tok(TT::LPAR), import_from_as_names),
        pair(maybe(tok(TT::COMMA)), tok(TT::RPAR)),
    )
    .or(left(import_from_as_names, not(tok(TT::COMMA))))
    .or(tok(TT::STAR).map(|t| {
        vec![ImportItem {
            name: t.into(),
            alias: None,
        }]
    }))
    .parse(input)
}

// import_from_as_names:
//     | ','.import_from_as_name+
fn import_from_as_names(input: &[Token]) -> ParseResult<Vec<ImportItem>> {
    sep_by(import_from_as_name, TT::COMMA).parse(input)
}

// import_from_as_name:
//     | NAME ['as' NAME ]
fn import_from_as_name(input: &[Token]) -> ParseResult<ImportItem> {
    pair(name, maybe(right(token(TT::KEYWORD, "as"), name)))
        .map(|(name, alias)| ImportItem { name, alias })
        .parse(input)
}

// dotted_as_names:
//     | ','.dotted_as_name+
fn dotted_as_names(input: &[Token]) -> ParseResult<Vec<Module>> {
    todo!()
}

// dotted_as_name:
//     | dotted_name ['as' NAME ]

// dotted_name:
//     | dotted_name '.' NAME
//     | NAME

fn dotted_name(input: &[Token]) -> ParseResult<Vec<Name>> {
    todo!()
}

// # COMPOUND STATEMENTS
// # ===================

// # Common elements
// # ---------------

// block:
//     | NEWLINE INDENT statements DEDENT
//     | simple_stmts
fn block(input: &[Token]) -> ParseResult<Vec<Statement>> {
    left(
        right(pair(tok(TT::NEWLINE), tok(TT::INDENT)), statements),
        tok(TT::DEDENT),
    )
    .or(simple_stmts)
    .parse(input)
}

// decorators: ('@' named_expression NEWLINE )+
fn decorators(input: &[Token]) -> ParseResult<Vec<Decorator>> {
    zero_or_more(
        right(tok(TT::AT), left(named_expression, tok(TT::NEWLINE))).map(|expr| Decorator(expr)),
    )
    .parse(input)
}

// # Class definitions
// # -----------------

// class_def:
//     | decorators class_def_raw
//     | class_def_raw
fn class_def(input: &[Token]) -> ParseResult<Statement> {
    pair(decorators, class_def_raw)
        .map(|(d, mut c)| {
            c.decorators = d;
            c
        })
        .or(class_def_raw)
        .map(|c| Statement::ClassDefinition(c))
        .parse(input)
}

// class_def_raw:
//     | 'class' NAME [type_params] ['(' [arguments] ')' ] ':' block
// TODO: type_params
fn class_def_raw(input: &[Token]) -> ParseResult<ClassDefinition> {
    pair(
        right(token(TT::KEYWORD, "class"), name),
        pair(
            maybe(right(tok(TT::LPAR), left(arguments, tok(TT::RPAR)))),
            right(tok(TT::COLON), block),
        ),
    )
    .map(|(name, (ancestors, body))| ClassDefinition {
        name,
        ancestors: ancestors.unwrap_or(Arguments::empty()),
        body,
        decorators: vec![],
    })
    .parse(input)
}

// # Function definitions
// # --------------------

// function_def:
//     | decorators function_def_raw
//     | function_def_raw
fn function_def(input: &[Token]) -> ParseResult<Statement> {
    pair(decorators, function_def_raw)
        .map(|(dec, fun)| Statement::FunctionDeclaration(fun, dec))
        .parse(input)
}

// function_def_raw:
//     | 'def' NAME [type_params] '(' [params] ')' ['->' expression ] ':' [func_type_comment] block
//     | ASYNC 'def' NAME [type_params] '(' [params] ')' ['->' expression ] ':' [func_type_comment] block

// #TODO: Incomplete
fn function_def_raw(input: &[Token]) -> ParseResult<FunctionDeclaration> {
    pair(
        pair(
            right(token(TT::KEYWORD, "def"), name),
            right(
                tok(TT::LPAR),
                left(params, pair(tok(TT::RPAR), tok(TT::COLON))),
            ),
        ),
        block,
    )
    .map(|((n, p), b)| FunctionDeclaration {
        name: n,
        parameters: p,
        code: b,
    })
    .parse(input)
}

// # Function parameters
// # -------------------
fn name(input: &[Token]) -> ParseResult<Name> {
    tok(TT::NAME).parse(input).into()
}

// params:
//     | parameters
fn params(input: &[Token]) -> ParseResult<Vec<Parameter>> {
    parameters.parse(input)
}

// parameters:
//     | slash_no_default param_no_default* param_with_default* [star_etc]
//     | slash_with_default param_with_default* [star_etc]
//     | param_no_default+ param_with_default* [star_etc]
//     | param_with_default+ [star_etc]
//     | star_etc
fn parameters(input: &[Token]) -> ParseResult<Vec<Parameter>> {
    pair(
        pair(slash_no_default, zero_or_more(param_no_default)),
        pair(zero_or_more(param_with_default), maybe(star_etc)),
    )
    .map(|((mut u, v), (w, o))| {
        u.extend(v);
        u.extend(w);
        u.extend(o.unwrap_or(vec![]));
        u
    })
    .or(pair(
        pair(slash_with_default, zero_or_more(param_with_default)),
        maybe(star_etc),
    )
    .map(|((mut u, v), o)| {
        u.extend(v);
        u.extend(o.unwrap_or(vec![]));
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
        u.extend(o.unwrap_or(vec![]));
        u
    }))
    .or(
        pair(one_or_more(param_with_default), maybe(star_etc)).map(|(mut u, o)| {
            u.extend(o.unwrap_or(vec![]));
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
fn slash_no_default(input: &[Token]) -> ParseResult<Vec<Parameter>> {
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
fn slash_with_default(input: &[Token]) -> ParseResult<Vec<Parameter>> {
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
fn star_etc(input: &[Token]) -> ParseResult<Vec<Parameter>> {
    pair(
        right(tok(TT::STAR), param_no_default),
        pair(zero_or_more(param_maybe_default), maybe(kwds)),
    )
    .map(|(v, (mut u, w))| {
        u.push(v);
        if let Some(p) = w {
            u.push(p)
        };
        u
    })
    .or(pair(
        right(tok(TT::STAR), param_no_default_star_annotation),
        pair(zero_or_more(param_maybe_default), maybe(kwds)),
    )
    .map(|(v, (mut u, w))| {
        u.push(v);
        if let Some(p) = w {
            u.push(p)
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
        if let Some(p) = v {
            u.push(p)
        };
        u
    }))
    .or(kwds.map(|p| vec![p]))
    .parse(input)
}

// kwds:
//     | '**' param_no_default
fn kwds(input: &[Token]) -> ParseResult<Parameter> {
    right(tok(TT::DOUBLESTAR), param_no_default)
        .map(|p| p.kwargs())
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
fn param_no_default(input: &[Token]) -> ParseResult<Parameter> {
    left(param, tok(TT::COMMA))
        .or(left(param, lookahead(tok(TT::RPAR))))
        .map(|n| n.into())
        .parse(input)
}

// param_no_default_star_annotation:
//     | param_star_annotation ',' TYPE_COMMENT?
//     | param_star_annotation TYPE_COMMENT? &')'
fn param_no_default_star_annotation(input: &[Token]) -> ParseResult<Parameter> {
    left(param_star_annotation, tok(TT::COMMA))
        .or(left(param_star_annotation, lookahead(tok(TT::RPAR))))
        .parse(input)
}

// param_with_default:
//     | param default ',' TYPE_COMMENT?
//     | param default TYPE_COMMENT? &')'
fn param_with_default(input: &[Token]) -> ParseResult<Parameter> {
    left(pair(param, default), tok(TT::COMMA))
        .or(left(pair(param, default), lookahead(tok(TT::RPAR))))
        .map(|(n, e)| Parameter::with_default(n, e))
        .parse(input)
}

// param_maybe_default:
//     | param default? ',' TYPE_COMMENT?
//     | param default? TYPE_COMMENT? &')'
fn param_maybe_default(input: &[Token]) -> ParseResult<Parameter> {
    pair(param, left(maybe(default), tok(TT::COMMA)))
        .map(|(n, o)| {
            if let Some(d) = o {
                Parameter::with_default(n, d)
            } else {
                n.into()
            }
        })
        .or(
            pair(param, left(maybe(default), lookahead(tok(TT::RPAR)))).map(|(n, o)| {
                if let Some(d) = o {
                    Parameter::with_default(n, d)
                } else {
                    n.into()
                }
            }),
        )
        .parse(input)
}

// param: NAME annotation?
fn param(input: &[Token]) -> ParseResult<Name> {
    name.parse(input)
}

// param_star_annotation: NAME star_annotation
fn param_star_annotation(input: &[Token]) -> ParseResult<Parameter> {
    pair(name, star_annotation)
        .map(|(n, a)| Parameter::with_annotation(n, a))
        .parse(input)
}

// annotation: ':' expression
fn annotation(input: &[Token]) -> ParseResult<Expression> {
    right(tok(TT::COLON), expression).parse(input)
}

// star_annotation: ':' star_expression
fn star_annotation(input: &[Token]) -> ParseResult<Expression> {
    right(tok(TT::COLON), star_expression).parse(input)
}

// default: '=' expression  | invalid_default
fn default(input: &[Token]) -> ParseResult<Expression> {
    right(tok(TT::EQUAL), expression).parse(input)
}

// # If statement
// # ------------

// if_stmt:
//     | 'if' named_expression ':' block elif_stmt
//     | 'if' named_expression ':' block [else_block]
fn if_stmt(input: &[Token]) -> ParseResult<Statement> {
    pair(
        right(token(TT::KEYWORD, "if"), named_expression),
        right(tok(TT::COLON), pair(block, elif_stmt)),
    )
    .map(|(expr, (then, (elif, els)))| Statement::If(expr, then, elif, els))
    .or(pair(
        right(token(TT::KEYWORD, "if"), named_expression),
        right(tok(TT::COLON), pair(block, maybe(else_block))),
    )
    .map(|(expr, (then, maybe_else))| Statement::If(expr, then, vec![], maybe_else)))
    .parse(input)
}

// elif_stmt:
//     | 'elif' named_expression ':' block elif_stmt
//     | 'elif' named_expression ':' block [else_block]
fn elif_stmt(
    input: &[Token],
) -> ParseResult<(Vec<(Expression, Vec<Statement>)>, Option<Vec<Statement>>)> {
    pair(
        one_or_more(pair(
            right(token(TT::KEYWORD, "elif"), named_expression),
            right(tok(TT::COLON), block),
        )),
        maybe(else_block),
    )
    .parse(input)
}

// else_block:
//     | 'else' ':' block
fn else_block(input: &[Token]) -> ParseResult<Vec<Statement>> {
    right(pair(token(TT::KEYWORD, "else"), tok(TT::COLON)), block).parse(input)
}

// # While statement
// # ---------------

// while_stmt:
//     | 'while' named_expression ':' block [else_block]

fn while_stmt(input: &[Token]) -> ParseResult<Statement> {
    pair(
        right(token(TT::KEYWORD, "while"), pair(named_expression, block)),
        maybe(else_block),
    )
    .map(|((e, b), els)| Statement::While(Box::new(e), b, els))
    .parse(input)
}

// # For statement
// # -------------

// for_stmt:
//     | 'for' star_targets 'in' ~ star_expressions ':' [TYPE_COMMENT] block [else_block]
//     | ASYNC 'for' star_targets 'in' ~ star_expressions ':' [TYPE_COMMENT] block [else_block]
// TODO: type comment, async, implement ~
fn for_stmt(input: &[Token]) -> ParseResult<Statement> {
    pair(
        left(
            right(token(TT::KEYWORD, "for"), star_targets),
            token(TT::KEYWORD, "in"),
        ),
        pair(
            left(star_expressions, tok(TT::COLON)),
            pair(block, maybe(else_block)),
        ),
    )
    .map(|(tgt, (expr, (blck, els)))| Statement::For(tgt, expr, blck, els))
    .parse(input)
}

// # With statement
// # --------------

// with_stmt:
//     | 'with' '(' ','.with_item+ ','? ')' ':' block
//     | 'with' ','.with_item+ ':' [TYPE_COMMENT] block
//     | ASYNC 'with' '(' ','.with_item+ ','? ')' ':' block
//     | ASYNC 'with' ','.with_item+ ':' [TYPE_COMMENT] block

// TODO: typing, async
fn with_stmt(input: &[Token]) -> ParseResult<Statement> {
    pair(
        right(
            pair(token(TT::KEYWORD, "with"), tok(TT::LPAR)),
            left(sep_by(with_item, TT::COMMA), maybe(tok(TT::COMMA))),
        ),
        right(pair(tok(TT::RPAR), tok(TT::COLON)), block),
    )
    .or(right(
        pair(token(TT::KEYWORD, "with"), tok(TT::COLON)),
        pair(sep_by(with_item, TT::COMMA), right(tok(TT::COLON), block)),
    ))
    .map(|(w, b)| Statement::With(w, b))
    .parse(input)
}

// with_item:
//     | expression 'as' star_target &(',' | ')' | ':')
//     | expression
fn with_item(input: &[Token]) -> ParseResult<Expression> {
    left(
        pair(expression, right(token(TT::KEYWORD, "as"), star_target)),
        lookahead(tok(TT::COMMA).or(tok(TT::RPAR)).or(tok(TT::COLON))),
    )
    .map(|(e, t)| Expression::WithItem(Box::new(e), Some(Box::new(t))))
    .or(expression.map(|e| Expression::WithItem(Box::new(e), None)))
    .parse(input)
}

// # Try statement
// # -------------

// try_stmt:
//     | 'try' ':' block finally_block
//     | 'try' ':' block except_block+ [else_block] [finally_block]
//     | 'try' ':' block except_star_block+ [else_block] [finally_block]
fn try_stmt(input: &[Token]) -> ParseResult<Statement> {
    right(
        pair(token(TT::KEYWORD, "try"), tok(TT::COLON)),
        pair(block, finally_block),
    )
    .map(|(b, f)| Statement::Try(b, vec![], None, Some(f)))
    .or(right(
        pair(token(TT::KEYWORD, "try"), tok(TT::COLON)),
        pair(
            pair(block, one_or_more(except_block)),
            pair(maybe(else_block), maybe(finally_block)),
        ),
    )
    .map(|((b, ex), (e, f))| Statement::Try(b, ex, e, f)))
    .or(right(
        pair(token(TT::KEYWORD, "try"), tok(TT::COLON)),
        pair(
            pair(block, one_or_more(except_star_block)),
            pair(maybe(else_block), maybe(finally_block)),
        ),
    )
    .map(|((b, ex), (e, f))| Statement::Try(b, ex, e, f)))
    .parse(input)
}

// # Except statement
// # ----------------

// except_block:
//     | 'except' expression ['as' NAME ] ':' block
//     | 'except' ':' block
fn except_block(input: &[Token]) -> ParseResult<Expression> {
    pair(
        right(
            token(TT::KEYWORD, "except"),
            pair(expression, maybe(right(token(TT::KEYWORD, "as"), name))),
        ),
        block,
    )
    .map(|((e, a), b)| Expression::ExceptBlock(Some(Box::new(e)), a, b, false))
    .or(
        right(pair(token(TT::KEYWORD, "except"), tok(TT::COLON)), block)
            .map(|b| Expression::ExceptBlock(None, None, b, false)),
    )
    .parse(input)
}

// except_star_block:
//     | 'except' '*' expression ['as' NAME ] ':' block
fn except_star_block(input: &[Token]) -> ParseResult<Expression> {
    pair(
        right(
            pair(token(TT::KEYWORD, "except"), tok(TT::STAR)),
            pair(expression, maybe(right(token(TT::KEYWORD, "as"), name))),
        ),
        block,
    )
    .map(|((e, a), b)| Expression::ExceptBlock(Some(Box::new(e)), a, b, true))
    .parse(input)
}

// finally_block:
//     | 'finally' ':' block
fn finally_block(input: &[Token]) -> ParseResult<Vec<Statement>> {
    right(pair(token(TT::KEYWORD, "finally"), tok(TT::COLON)), block).parse(input)
}

// # Match statement
// # ---------------

// match_stmt:
//     | "match" subject_expr ':' NEWLINE INDENT case_block+ DEDENT
fn match_stmt(input: &[Token]) -> ParseResult<Statement> {
    todo!()
}

// subject_expr:
//     | star_named_expression ',' star_named_expressions?
//     | named_expression

// case_block:
//     | "case" patterns guard? ':' block

// guard: 'if' named_expression

// patterns:
//     | open_sequence_pattern
//     | pattern

// pattern:
//     | as_pattern
//     | or_pattern

// as_pattern:
//     | or_pattern 'as' pattern_capture_target

// or_pattern:
//     | '|'.closed_pattern+

// closed_pattern:
//     | literal_pattern
//     | capture_pattern
//     | wildcard_pattern
//     | value_pattern
//     | group_pattern
//     | sequence_pattern
//     | mapping_pattern
//     | class_pattern

// # Literal patterns are used for equality and identity constraints
// literal_pattern:
//     | signed_number !('+' | '-')
//     | complex_number
//     | strings
//     | 'None'
//     | 'True'
//     | 'False'

// # Literal expressions are used to restrict permitted mapping pattern keys
// literal_expr:
//     | signed_number !('+' | '-')
//     | complex_number
//     | strings
//     | 'None'
//     | 'True'
//     | 'False'

// complex_number:
//     | signed_real_number '+' imaginary_number
//     | signed_real_number '-' imaginary_number

// signed_number:
//     | NUMBER
//     | '-' NUMBER

// signed_real_number:
//     | real_number
//     | '-' real_number

// real_number:
//     | NUMBER

// imaginary_number:
//     | NUMBER

// capture_pattern:
//     | pattern_capture_target

// pattern_capture_target:
//     | !"_" NAME !('.' | '(' | '=')

// wildcard_pattern:
//     | "_"

// value_pattern:
//     | attr !('.' | '(' | '=')

// attr:
//     | name_or_attr '.' NAME

// name_or_attr:
//     | attr
//     | NAME

// group_pattern:
//     | '(' pattern ')'

// sequence_pattern:
//     | '[' maybe_sequence_pattern? ']'
//     | '(' open_sequence_pattern? ')'

// open_sequence_pattern:
//     | maybe_star_pattern ',' maybe_sequence_pattern?

// maybe_sequence_pattern:
//     | ','.maybe_star_pattern+ ','?

// maybe_star_pattern:
//     | star_pattern
//     | pattern

// star_pattern:
//     | '*' pattern_capture_target
//     | '*' wildcard_pattern

// mapping_pattern:
//     | '{' '}'
//     | '{' double_star_pattern ','? '}'
//     | '{' items_pattern ',' double_star_pattern ','? '}'
//     | '{' items_pattern ','? '}'

// items_pattern:
//     | ','.key_value_pattern+

// key_value_pattern:
//     | (literal_expr | attr) ':' pattern

// double_star_pattern:
//     | '**' pattern_capture_target

// class_pattern:
//     | name_or_attr '(' ')'
//     | name_or_attr '(' positional_patterns ','? ')'
//     | name_or_attr '(' keyword_patterns ','? ')'
//     | name_or_attr '(' positional_patterns ',' keyword_patterns ','? ')'

// positional_patterns:
//     | ','.pattern+

// keyword_patterns:
//     | ','.keyword_pattern+

// keyword_pattern:
//     | NAME '=' pattern

// # Type statement
// # ---------------

// type_alias:
//     | "type" NAME [type_params] '=' expression

fn type_alias(input: &[Token]) -> ParseResult<Statement> {
    todo!()
    // pair(right(token(TT::SOFT_KEYWORD, "type"), name), pair(left(maybe(type_params), tok(TT::EQUAL)), expression)).parse(input)
}

// # Type parameter declaration
// # --------------------------

// type_params: '[' type_param_seq  ']'

// type_param_seq: ','.type_param+ [',']

// type_param:
//     | NAME [type_param_bound]
//     | '*' NAME ':' expression
//     | '*' NAME
//     | '**' NAME ':' expression
//     | '**' NAME

// type_param_bound: ':' expression

// # EXPRESSIONS
// # -----------

// expressions:
//     | expression (',' expression )+ [',']
//     | expression ','
//     | expression
fn expressions(input: &[Token]) -> ParseResult<Vec<Expression>> {
    todo!()
}

// expression:
//     | disjunction 'if' disjunction 'else' expression
//     | disjunction
//     | lambdef
// TODO: lambda
fn expression(input: &[Token]) -> ParseResult<Expression> {
    pair(
        left(disjunction, token(TT::KEYWORD, "if")),
        pair(left(disjunction, token(TT::KEYWORD, "else")), expression),
    )
    .map(|(t, (c, e))| Expression::Ternary(Box::new(c), Box::new(t), Box::new(e)))
    .or(disjunction)
    // .or(lambdef)
    .parse(input)
}

// yield_expr:
//     | 'yield' 'from' expression
//     | 'yield' [star_expressions]
fn yield_expr(input: &[Token]) -> ParseResult<Expression> {
    right(
        pair(token(TT::KEYWORD, "yield"), token(TT::KEYWORD, "from")),
        expression,
    )
    .map(|e| Expression::YieldFrom(Box::new(e)))
    .or(right(
        token(TT::KEYWORD, "yield"),
        maybe(star_expressions).map(|v| v.unwrap_or(vec![])),
    )
    .map(|e| Expression::Yield(e)))
    .parse(input)
}

// star_expressions:
//     | star_expression (',' star_expression )+ [',']
//     | star_expression ','
//     | star_expression
fn star_expressions(input: &[Token]) -> ParseResult<Vec<Expression>> {
    left(sep_by(star_expression, TT::COMMA), maybe(tok(TT::COMMA))).parse(input)
}

// star_expression:
//     | '*' bitwise_or
//     | expression
fn star_expression(input: &[Token]) -> ParseResult<Expression> {
    pair(tok(TT::STAR), bitwise_or)
        .map(|(_, e)| Expression::Starred(Box::new(e)))
        .or(expression)
        .parse(input)
}

// star_named_expressions: ','.star_named_expression+ [',']
fn star_named_expressions(input: &[Token]) -> ParseResult<Vec<Expression>> {
    left(
        sep_by(star_named_expression, TT::COMMA),
        maybe(tok(TT::COMMA)),
    )
    .parse(input)
}

// star_named_expression:
//     | '*' bitwise_or
//     | named_expression
fn star_named_expression(input: &[Token]) -> ParseResult<Expression> {
    right(tok(TT::STAR), bitwise_or)
        .or(named_expression)
        .parse(input)
}

// assignment_expression:
//     | NAME ':=' ~ expression
fn assignment_expression(input: &[Token]) -> ParseResult<Expression> {
    pair(
        left(name.map(|n| Expression::Name(n)), tok(TT::COLONEQUAL)),
        expression,
    )
    .map(|(l, r)| Expression::Walrus(Box::new(l), Box::new(r)))
    .parse(input)
}

// named_expression:
//     | assignment_expression
//     | expression !':='
fn named_expression(input: &[Token]) -> ParseResult<Expression> {
    assignment_expression
        .or(left(expression, not(tok(TT::COLONEQUAL))))
        .parse(input)
}

// disjunction:
//     | conjunction ('or' conjunction )+
//     | conjunction
fn disjunction(input: &[Token]) -> ParseResult<Expression> {
    pair(conjunction, right(token(TT::KEYWORD, "or"), conjunction))
        .map(|(l, r)| Expression::BinaryOperation(Operator::Or, Box::new((l, r))))
        .or(conjunction)
        .parse(input)
}

// conjunction:
//     | inversion ('and' inversion )+
//     | inversion
fn conjunction(input: &[Token]) -> ParseResult<Expression> {
    pair(inversion, right(token(TT::KEYWORD, "and"), inversion))
        .map(|(l, r)| Expression::BinaryOperation(Operator::And, Box::new((l, r))))
        .or(inversion)
        .parse(input)
}

// inversion:
//     | 'not' inversion
//     | comparison
fn inversion(input: &[Token]) -> ParseResult<Expression> {
    right(token(TT::KEYWORD, "not"), inversion)
        .map(|i| Expression::UnaryOperation(Operator::Not, Box::new(i)))
        .or(comparison)
        .parse(input)
}

// # Comparison operators
// # --------------------

// comparison:
//     | bitwise_or compare_op_bitwise_or_pair+
//     | bitwise_or
fn comparison(input: &[Token]) -> ParseResult<Expression> {
    pair(bitwise_or, one_or_more(compare_op_bitwise_or_pair))
        .map(|(l, r)| Expression::Comparison(Box::new(l), r))
        .or(bitwise_or)
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
fn compare_op_bitwise_or_pair(input: &[Token]) -> ParseResult<Expression> {
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
fn eq_bitwise_or(input: &[Token]) -> ParseResult<Expression> {
    right(tok(TT::EQEQUAL), bitwise_or).parse(input)
}

// noteq_bitwise_or:
//     | ('!=' ) bitwise_or
fn noteq_bitwise_or(input: &[Token]) -> ParseResult<Expression> {
    right(tok(TT::NOTEQUAL), bitwise_or).parse(input)
}

// lte_bitwise_or: '<=' bitwise_or
fn lte_bitwise_or(input: &[Token]) -> ParseResult<Expression> {
    right(tok(TT::LESSEQUAL), bitwise_or).parse(input)
}

// lt_bitwise_or: '<' bitwise_or
fn lt_bitwise_or(input: &[Token]) -> ParseResult<Expression> {
    right(tok(TT::LESS), bitwise_or).parse(input)
}

// gte_bitwise_or: '>=' bitwise_or
fn gte_bitwise_or(input: &[Token]) -> ParseResult<Expression> {
    right(tok(TT::GREATEREQUAL), bitwise_or).parse(input)
}

// gt_bitwise_or: '>' bitwise_or
fn gt_bitwise_or(input: &[Token]) -> ParseResult<Expression> {
    right(tok(TT::GREATER), bitwise_or).parse(input)
}

// notin_bitwise_or: 'not' 'in' bitwise_or
fn notin_bitwise_or(input: &[Token]) -> ParseResult<Expression> {
    right(
        pair(token(TT::KEYWORD, "not"), token(TT::KEYWORD, "in")),
        bitwise_or,
    )
    .parse(input)
}

// in_bitwise_or: 'in' bitwise_or
fn in_bitwise_or(input: &[Token]) -> ParseResult<Expression> {
    right(token(TT::KEYWORD, "in"), bitwise_or).parse(input)
}

// isnot_bitwise_or: 'is' 'not' bitwise_or
fn isnot_bitwise_or(input: &[Token]) -> ParseResult<Expression> {
    right(
        pair(token(TT::KEYWORD, "is"), token(TT::KEYWORD, "not")),
        bitwise_or,
    )
    .parse(input)
}

// is_bitwise_or: 'is' bitwise_or
fn is_bitwise_or(input: &[Token]) -> ParseResult<Expression> {
    right(token(TT::KEYWORD, "is"), bitwise_or).parse(input)
}

// # Bitwise operators
// # -----------------

// bitwise_or_tail:
//     | '|' bitwise_xor bitwise_or_tail
//     | epsilon
fn bitwise_or_tail(input: &[Token]) -> ParseResult<IncompleteExpression> {
    right(tok(TT::VBAR), pair(bitwise_xor, bitwise_or_tail))
        .map(|(e, t)| {
            IncompleteExpression::BinaryOperation(Operator::BitwiseOr, Box::new(e), Box::new(t))
        })
        .or(epsilon.map(|_| IncompleteExpression::Empty))
        .parse(input)
}

// bitwise_or:
//     | bitwise_xor bitwise_or_tail
fn bitwise_or(input: &[Token]) -> ParseResult<Expression> {
    pair(bitwise_xor, bitwise_or_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::BinaryOperation(_, ref expr, ref tail) => (
                        Expression::BinaryOperation(
                            Operator::BitwiseOr,
                            Box::new((current_expr, *expr.clone())),
                        ),
                        tail.clone(),
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
fn bitwise_xor_tail(input: &[Token]) -> ParseResult<IncompleteExpression> {
    right(tok(TT::CIRCUMFLEX), pair(bitwise_and, bitwise_xor_tail))
        .map(|(e, t)| {
            IncompleteExpression::BinaryOperation(Operator::BitwiseXor, Box::new(e), Box::new(t))
        })
        .or(epsilon.map(|_| IncompleteExpression::Empty))
        .parse(input)
}

// bitwise_xor:
//     | bitwise_and bitwise_xor_tail
fn bitwise_xor(input: &[Token]) -> ParseResult<Expression> {
    pair(bitwise_and, bitwise_xor_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::BinaryOperation(_, ref expr, ref tail) => (
                        Expression::BinaryOperation(
                            Operator::BitwiseXor,
                            Box::new((current_expr, *expr.clone())),
                        ),
                        tail.clone(),
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
fn bitwise_and_tail(input: &[Token]) -> ParseResult<IncompleteExpression> {
    right(tok(TT::VBAR), pair(shift_expr, bitwise_and_tail))
        .map(|(e, t)| {
            IncompleteExpression::BinaryOperation(Operator::BitwiseAnd, Box::new(e), Box::new(t))
        })
        .or(epsilon.map(|_| IncompleteExpression::Empty))
        .parse(input)
}

// bitwise_and:
//     | shift_expr bitwise_and_tail
fn bitwise_and(input: &[Token]) -> ParseResult<Expression> {
    pair(shift_expr, bitwise_and_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::BinaryOperation(_, ref expr, ref tail) => (
                        Expression::BinaryOperation(
                            Operator::BitwiseAnd,
                            Box::new((current_expr, *expr.clone())),
                        ),
                        tail.clone(),
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
fn shift_expr_tail(input: &[Token]) -> ParseResult<IncompleteExpression> {
    right(tok(TT::LEFTSHIFT), pair(sum, shift_expr_tail))
        .map(|(e, t)| {
            IncompleteExpression::BinaryOperation(Operator::LeftShift, Box::new(e), Box::new(t))
        })
        .or(
            right(tok(TT::RIGHTSHIFT), pair(sum, shift_expr_tail)).map(|(e, t)| {
                IncompleteExpression::BinaryOperation(
                    Operator::RightShift,
                    Box::new(e),
                    Box::new(t),
                )
            }),
        )
        .or(epsilon.map(|_| IncompleteExpression::Empty))
        .parse(input)
}

// shift_expr:
//     | sum shift_expr_tail
fn shift_expr(input: &[Token]) -> ParseResult<Expression> {
    pair(sum, shift_expr_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::BinaryOperation(op, ref expr, ref tail) => (
                        Expression::BinaryOperation(op, Box::new((current_expr, *expr.clone()))),
                        tail.clone(),
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
fn sum_tail(input: &[Token]) -> ParseResult<IncompleteExpression> {
    right(tok(TT::PLUS), pair(term, sum_tail))
        .map(|(e, t)| {
            IncompleteExpression::BinaryOperation(Operator::Plus, Box::new(e), Box::new(t))
        })
        .or(right(tok(TT::MINUS), pair(term, sum_tail)).map(|(e, t)| {
            IncompleteExpression::BinaryOperation(Operator::Minus, Box::new(e), Box::new(t))
        }))
        .or(epsilon.map(|_| IncompleteExpression::Empty))
        .parse(input)
}

// sum:
//     | term sum_tail
fn sum(input: &[Token]) -> ParseResult<Expression> {
    pair(term, sum_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::BinaryOperation(op, ref expr, ref tail) => (
                        Expression::BinaryOperation(op, Box::new((current_expr, *expr.clone()))),
                        tail.clone(),
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
fn term_tail(input: &[Token]) -> ParseResult<IncompleteExpression> {
    right(tok(TT::STAR), pair(factor, term_tail))
        .map(|(e, t)| {
            IncompleteExpression::BinaryOperation(Operator::Times, Box::new(e), Box::new(t))
        })
        .or(
            right(tok(TT::SLASH), pair(factor, term_tail)).map(|(e, t)| {
                IncompleteExpression::BinaryOperation(Operator::Divide, Box::new(e), Box::new(t))
            }),
        )
        .or(
            right(tok(TT::DOUBLESLASH), pair(factor, term_tail)).map(|(e, t)| {
                IncompleteExpression::BinaryOperation(Operator::IntDivide, Box::new(e), Box::new(t))
            }),
        )
        .or(
            right(tok(TT::PERCENT), pair(factor, term_tail)).map(|(e, t)| {
                IncompleteExpression::BinaryOperation(Operator::Modulo, Box::new(e), Box::new(t))
            }),
        )
        .or(right(tok(TT::AT), pair(factor, term_tail)).map(|(e, t)| {
            IncompleteExpression::BinaryOperation(Operator::MatrixMul, Box::new(e), Box::new(t))
        }))
        .or(epsilon.map(|_| IncompleteExpression::Empty))
        .parse(input)
}

// term:
//     | factor term_tail
fn term(input: &[Token]) -> ParseResult<Expression> {
    pair(factor, term_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::BinaryOperation(op, ref expr, ref tail) => (
                        Expression::BinaryOperation(op, Box::new((current_expr, *expr.clone()))),
                        tail.clone(),
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
fn factor(input: &[Token]) -> ParseResult<Expression> {
    power
        .or(right(tok(TT::PLUS), factor))
        .or(pair(tok(TT::MINUS), factor)
            .map(|(o, e)| Expression::UnaryOperation(o.into(), Box::new(e))))
        .or(pair(tok(TT::TILDE), factor)
            .map(|(o, e)| Expression::UnaryOperation(o.into(), Box::new(e))))
        .parse(input)
}

// power:
//     | await_primary '**' factor
//     | await_primary
fn power(input: &[Token]) -> ParseResult<Expression> {
    pair(await_primary, pair(tok(TT::DOUBLESTAR), factor))
        .map(|(l, (o, r))| Expression::BinaryOperation(o.into(), Box::new((l, r))))
        .or(await_primary)
        .parse(input)
}

// # Primary elements
// # ----------------

// # Primary elements are things like "obj.something.something", "obj[something]", "obj(something)", "obj" ...

// await_primary:
//     | AWAIT primary
//     | primary
fn await_primary(input: &[Token]) -> ParseResult<Expression> {
    right(token(TT::KEYWORD, "await"), primary)
        .or(primary)
        .parse(input)
}

// primary:
//   | atom n_primary
fn primary(input: &[Token]) -> ParseResult<Expression> {
    pair(atom, n_primary)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::Call(ref args, ref tail) => (
                        Expression::Call(Box::new(current_expr), args.clone()),
                        tail.clone(),
                    ),
                    IncompleteExpression::Slice(ref slice, ref tail) => (
                        Expression::Slice(Box::new(current_expr), slice.clone()),
                        tail.clone(),
                    ),
                    IncompleteExpression::Subscript(ref name, ref tail) => (
                        Expression::Subscript(Box::new(current_expr), name.clone()),
                        tail.clone(),
                    ),
                    IncompleteExpression::PrimaryGenexp(ref genexp, ref tail) => (
                        Expression::PrimaryGenexp(Box::new(current_expr), genexp.clone()),
                        tail.clone(),
                    ),
                    _ => unreachable!(),
                };
            }
            current_expr
        })
        .parse(input)
}

// n_primary:
//     | '.' NAME n_primary
//     | genexp n_primary
//     | '(' [arguments] ')' n_primary
//     | '[' slices ']' n_primary
//     | epsilon
fn n_primary(input: &[Token]) -> ParseResult<IncompleteExpression> {
    pair(right(tok(TT::DOT), name), n_primary)
        .map(|(n, tail)| IncompleteExpression::Subscript(n, Box::new(tail)))
        .or(pair(genexp, n_primary)
            .map(|(g, tail)| IncompleteExpression::PrimaryGenexp(Box::new(g), Box::new(tail))))
        .or(pair(
            right(tok(TT::LPAR), left(arguments, tok(TT::RPAR))),
            n_primary,
        )
        .map(|(a, tail)| IncompleteExpression::Call(a, Box::new(tail))))
        .or(
            pair(right(tok(TT::LSQB), left(slices, tok(TT::RSQB))), n_primary)
                .map(|(n, tail)| IncompleteExpression::Slice(n, Box::new(tail))),
        )
        .or(epsilon.map(|_| IncompleteExpression::Empty))
        .parse(input)
}

// slices:
//     | slice !','
//     | ','.(slice | starred_expression)+ [',']
fn slices(input: &[Token]) -> ParseResult<Vec<Slice>> {
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
fn slice(input: &[Token]) -> ParseResult<Slice> {
    pair(
        maybe(expression),
        pair(
            right(tok(TT::COLON), maybe(expression)),
            maybe(right(tok(TT::COLON), maybe(expression))),
        ),
    )
    .map(|(l, (m, r))| Slice::Delimited(l, m, r.flatten()))
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
fn atom(input: &[Token]) -> ParseResult<Expression> {
    name.map(|n| Expression::Name(n))
        .or(token(TT::KEYWORD, "True").map(|_| Expression::True))
        .or(token(TT::KEYWORD, "False").map(|_| Expression::False))
        .or(token(TT::KEYWORD, "None").map(|_| Expression::None))
        .or(strings)
        .or(number.map(|n| Expression::Number(n)))
        .or(tuple.or(group).or(genexp))
        .or(list.or(listcomp))
        .or(dict.or(set).or(dictcomp).or(setcomp))
        .or(tok(TT::ELLIPSIS).map(|_| Expression::Ellipsis))
        .parse(input)
}

fn number(input: &[Token]) -> ParseResult<Number> {
    tok(TT::NUMBER).parse(input).into()
}

// group:
//     | '(' (yield_expr | named_expression) ')'
fn group(input: &[Token]) -> ParseResult<Expression> {
    right(
        tok(TT::LPAR),
        left(yield_expr.or(named_expression), tok(TT::RPAR)),
    )
    .parse(input)
}

// # Lambda functions
// # ----------------

// lambdef:
//     | 'lambda' [lambda_params] ':' expression

// lambda_params:
//     | lambda_parameters

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

// lambda_slash_no_default:
//     | lambda_param_no_default+ '/' ','
//     | lambda_param_no_default+ '/' &':'

// lambda_slash_with_default:
//     | lambda_param_no_default* lambda_param_with_default+ '/' ','
//     | lambda_param_no_default* lambda_param_with_default+ '/' &':'

// lambda_star_etc:
//     | '*' lambda_param_no_default lambda_param_maybe_default* [lambda_kwds]
//     | '*' ',' lambda_param_maybe_default+ [lambda_kwds]
//     | lambda_kwds

// lambda_kwds:
//     | '**' lambda_param_no_default

// lambda_param_no_default:
//     | lambda_param ','
//     | lambda_param &':'
// lambda_param_with_default:
//     | lambda_param default ','
//     | lambda_param default &':'
// lambda_param_maybe_default:
//     | lambda_param default? ','
//     | lambda_param default? &':'
// lambda_param: NAME

// # LITERALS
// # ========

// fstring_middle:
//     | fstring_replacement_field
//     | FSTRING_MIDDLE
fn fstring_middle(input: &[Token]) -> ParseResult<PyString> {
    todo!()
}

// fstring_replacement_field:
//     | '{' (yield_expr | star_expressions) '='? [fstring_conversion] [fstring_full_format_spec] '}'
fn fstring_replacement_field(input: &[Token]) -> ParseResult<Expression> {
    todo!()
}

// fstring_conversion:
//     | "!" NAME
fn fstring_conversion(input: &[Token]) -> ParseResult<Expression> {
    todo!()
}

// fstring_full_format_spec:
//     | ':' fstring_format_spec*
fn fstring_full_format_spec(input: &[Token]) -> ParseResult<Expression> {
    todo!()
}

// fstring_format_spec:
//     | FSTRING_MIDDLE
//     | fstring_replacement_field
fn fstring_format_spec(input: &[Token]) -> ParseResult<Expression> {
    todo!()
}

// fstring:
//     | FSTRING_START fstring_middle* FSTRING_END
fn fstring(input: &[Token]) -> ParseResult<Vec<PyString>> {
    left(
        right(tok(TT::FSTRING_START), zero_or_more(fstring_middle)),
        tok(TT::FSTRING_END),
    )
    .parse(input)
}

// string: STRING
fn string(input: &[Token]) -> ParseResult<Token> {
    tok(TT::STRING).parse(input)
}

// strings: (fstring|string)+
fn strings(input: &[Token]) -> ParseResult<Expression> {
    one_or_more(string.map(|t| PyString::Literal(t.lexeme)))
        // .or(fstring))
        .map(|v| Expression::Strings(v))
        .parse(input)
}

// list:
//     | '[' [star_named_expressions] ']'
fn list(input: &[Token]) -> ParseResult<Expression> {
    left(
        right(tok(TT::LSQB), maybe(star_named_expressions)),
        tok(TT::RSQB),
    )
    .map(|e| Expression::List(e.unwrap_or(vec![])))
    .parse(input)
}

// tuple:
//     | '(' [star_named_expression ',' [star_named_expressions]  ] ')'
fn tuple(input: &[Token]) -> ParseResult<Expression> {
    left(
        right(
            tok(TT::LPAR),
            maybe(pair(
                left(star_named_expression, tok(TT::COMMA)),
                maybe(star_named_expressions),
            )),
        ),
        tok(TT::RPAR),
    )
    .map(|t| {
        let mut v = Vec::new();
        if let Some((e, es)) = t {
            v.push(e);
            v.extend(es.unwrap_or(vec![]));
        }
        Expression::Tuple(v)
    })
    .parse(input)
}

// set: '{' star_named_expressions '}'
fn set(input: &[Token]) -> ParseResult<Expression> {
    left(
        right(tok(TT::LBRACE), star_named_expressions),
        tok(TT::RBRACE),
    )
    .map(|e| Expression::Set(e))
    .parse(input)
}

// # Dicts
// # -----

// dict:
//     | '{' [double_starred_kvpairs] '}'
fn dict(input: &[Token]) -> ParseResult<Expression> {
    left(
        right(tok(TT::LBRACE), maybe(double_starred_kvpairs)),
        tok(TT::RBRACE),
    )
    .map(|e| Expression::Dict(e.unwrap_or(vec![])))
    .parse(input)
}

// double_starred_kvpairs: ','.double_starred_kvpair+ [',']
fn double_starred_kvpairs(input: &[Token]) -> ParseResult<Vec<Expression>> {
    left(
        sep_by(double_starred_kvpair, TT::COMMA),
        maybe(tok(TT::COMMA)),
    )
    .parse(input)
}

// double_starred_kvpair:
//     | '**' bitwise_or
//     | kvpair
fn double_starred_kvpair(input: &[Token]) -> ParseResult<Expression> {
    right(tok(TT::DOUBLESTAR), bitwise_or)
        .map(|e| Expression::DictUnwrap(Box::new(e)))
        .or(kvpair)
        .parse(input)
}

// kvpair: expression ':' expression
fn kvpair(input: &[Token]) -> ParseResult<Expression> {
    pair(expression, right(tok(TT::COLON), expression))
        .map(|(e, f)| Expression::Tuple(vec![e, f]))
        .parse(input)
}

// # Comprehensions & Generators
// # ---------------------------

// for_if_clauses:
//     | for_if_clause+
fn for_if_clauses(input: &[Token]) -> ParseResult<Vec<Expression>> {
    one_or_more(for_if_clause).parse(input)
}

// for_if_clause:
//     | ASYNC 'for' star_targets 'in' ~ disjunction ('if' disjunction )*
//     | 'for' star_targets 'in' ~ disjunction ('if' disjunction )*
// TODO: async
fn for_if_clause(input: &[Token]) -> ParseResult<Expression> {
    pair(
        pair(
            right(token(TT::KEYWORD, "for"), star_targets),
            right(token(TT::KEYWORD, "in"), disjunction),
        ),
        zero_or_more(right(token(TT::KEYWORD, "if"), disjunction)),
    )
    .map(|((tgt, set), ifs)| Expression::ForIfClause(tgt, Box::new(set), ifs))
    .parse(input)
}

// listcomp:
//     | '[' named_expression for_if_clauses ']'
fn listcomp(input: &[Token]) -> ParseResult<Expression> {
    pair(
        right(tok(TT::LSQB), star_named_expression),
        left(for_if_clauses, tok(TT::RSQB)),
    )
    .map(|(e, f)| Expression::ListComprehension(Box::new(e), f))
    .parse(input)
}

// setcomp:
//     | '{' named_expression for_if_clauses '}'
fn setcomp(input: &[Token]) -> ParseResult<Expression> {
    pair(
        right(tok(TT::LBRACE), named_expression),
        left(for_if_clauses, tok(TT::RBRACE)),
    )
    .map(|(e, f)| Expression::SetComprehension(Box::new(e), f))
    .parse(input)
}

// genexp:
//     | '(' ( assignment_expression | expression !':=') for_if_clauses ')'
fn genexp(input: &[Token]) -> ParseResult<Expression> {
    pair(
        right(
            tok(TT::LPAR),
            assignment_expression.or(left(expression, not(tok(TT::COLONEQUAL)))),
        ),
        left(for_if_clauses, tok(TT::RPAR)),
    )
    .map(|(e, f)| Expression::Generator(Box::new(e), f))
    .parse(input)
}

// dictcomp:
//     | '{' kvpair for_if_clauses '}'
fn dictcomp(input: &[Token]) -> ParseResult<Expression> {
    pair(
        right(tok(TT::LBRACE), kvpair),
        left(for_if_clauses, tok(TT::RSQB)),
    )
    .map(|(e, f)| Expression::DictComprehension(Box::new(e), f))
    .parse(input)
}

// # FUNCTION CALL ARGUMENTS
// # =======================

// arguments:
//     | args [','] &')'
fn arguments(input: &[Token]) -> ParseResult<Arguments> {
    left(args, pair(maybe(tok(TT::COMMA)), lookahead(tok(TT::RPAR)))).parse(input)
}

// args:
//     | ','.(starred_expression | ( assignment_expression | expression !':=') !'=')+ [',' kwargs ]
//     | kwargs
fn args(input: &[Token]) -> ParseResult<Arguments> {
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
        positional: pos.iter().map(|x| x.clone().into()).collect(),
        keyword: kw.iter().map(|x| x.clone().into()).collect(),
    })
    .parse(input)
}

// kwargs:
//     | ','.kwarg_or_starred+ ',' ','.kwarg_or_double_starred+
//     | ','.kwarg_or_starred+
//     | ','.kwarg_or_double_starred+
fn kwargs(input: &[Token]) -> ParseResult<Expression> {
    todo!()
}

// starred_expression:
//     | '*' expression
fn starred_expression(input: &[Token]) -> ParseResult<Expression> {
    right(tok(TT::STAR), expression)
        .map(|e| Expression::Starred(Box::new(e)))
        .parse(input)
}

// kwarg_or_starred:
//     | NAME '=' expression
//     | starred_expression
fn kwarg_or_starred(input: &[Token]) -> ParseResult<Expression> {
    todo!()
}

// kwarg_or_double_starred:
//     | NAME '=' expression
//     | '**' expression
fn kwarg_or_double_starred(input: &[Token]) -> ParseResult<Expression> {
    todo!()
}

// # ASSIGNMENT TARGETS
// # ==================

// # Generic targets
// # ---------------

// # NOTE: star_targets may contain *bitwise_or, targets may not.
// star_targets:
//     | star_target !','
//     | star_target (',' star_target )* [',']
fn star_targets(input: &[Token]) -> ParseResult<Vec<Expression>> {
    left(sep_by(star_target, TT::COMMA), maybe(tok(TT::COMMA))).parse(input)
}

// star_targets_list_seq: ','.star_target+ [',']
fn star_targets_list_seq(input: &[Token]) -> ParseResult<Expression> {
    todo!()
}

// star_targets_tuple_seq:
//     | star_target (',' star_target )+ [',']
//     | star_target ','
fn star_targets_tuple_seq(input: &[Token]) -> ParseResult<Expression> {
    todo!()
}

// star_target:
//     | '*' (!'*' star_target)
//     | target_with_star_atom
fn star_target(input: &[Token]) -> ParseResult<Expression> {
    right(tok(TT::STAR), right(not(tok(TT::STAR)), star_target))
        .or(target_with_star_atom)
        .parse(input)
}

// target_with_star_atom:
//     | t_primary !t_lookahead
//     | star_atom
fn target_with_star_atom(input: &[Token]) -> ParseResult<Expression> {
    left(t_primary, not(t_lookahead)).or(star_atom).parse(input)
}

// star_atom:
//     | NAME
//     | '(' target_with_star_atom ')'
//     | '(' [star_targets_tuple_seq] ')'
//     | '[' [star_targets_list_seq] ']'
fn star_atom(input: &[Token]) -> ParseResult<Expression> {
    name.map(|n| Expression::Name(n))
        .or(left(
            right(tok(TT::LPAR), target_with_star_atom),
            tok(TT::RPAR),
        ))
        .or(left(
            right(tok(TT::LPAR), star_targets_tuple_seq),
            tok(TT::RPAR),
        ))
        .or(left(
            right(tok(TT::LPAR), star_targets_list_seq),
            tok(TT::RPAR),
        ))
        .parse(input)
}

// single_target:
//     | single_subscript_attribute_target
//     | NAME
//     | '(' single_target ')'
fn single_target(input: &[Token]) -> ParseResult<Expression> {
    single_subscript_attribute_target
        .or(name.map(|n| Expression::Name(n)))
        .or(left(right(tok(TT::LPAR), single_target), tok(TT::RPAR)))
        .parse(input)
}

// single_subscript_attribute_target:
//     | t_primary !t_lookahead
fn single_subscript_attribute_target(input: &[Token]) -> ParseResult<Expression> {
    left(t_primary, not(t_lookahead)).parse(input)
}

// t_primary:
//     | atom t_primary_tail
fn t_primary(input: &[Token]) -> ParseResult<Expression> {
    pair(atom, t_primary_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::Call(ref args, ref tail) => (
                        Expression::Call(Box::new(current_expr), args.clone()),
                        tail.clone(),
                    ),
                    IncompleteExpression::Slice(ref slice, ref tail) => (
                        Expression::Slice(Box::new(current_expr), slice.clone()),
                        tail.clone(),
                    ),
                    IncompleteExpression::Subscript(ref name, ref tail) => (
                        Expression::Subscript(Box::new(current_expr), name.clone()),
                        tail.clone(),
                    ),
                    IncompleteExpression::PrimaryGenexp(ref genexp, ref tail) => (
                        Expression::PrimaryGenexp(Box::new(current_expr), genexp.clone()),
                        tail.clone(),
                    ),
                    _ => unreachable!(),
                };
            }
            current_expr
        })
        .parse(input)
}

// t_primary_tail:
//     | '.' NAME t_primary_tail
//     | '[' slices ']' t_primary_tail
//     | genexp t_primary_tail
//     | '(' [arguments] ')' t_primary_tail
//     | epsilon
fn t_primary_tail(input: &[Token]) -> ParseResult<IncompleteExpression> {
    pair(right(tok(TT::DOT), name), t_primary_tail)
        .map(|(n, tail)| IncompleteExpression::Subscript(n, Box::new(tail)))
        .or(pair(
            right(tok(TT::LSQB), left(slices, tok(TT::RSQB))),
            t_primary_tail,
        )
        .map(|(s, tail)| IncompleteExpression::Slice(s, Box::new(tail))))
        .or(pair(genexp, t_primary_tail)
            .map(|(g, tail)| IncompleteExpression::PrimaryGenexp(Box::new(g), Box::new(tail))))
        .or(pair(
            right(tok(TT::LPAR), left(arguments, tok(TT::RPAR))),
            t_primary_tail,
        )
        .map(|(n, tail)| IncompleteExpression::Call(n, Box::new(tail))))
        .or(epsilon.map(|_| IncompleteExpression::Empty))
        .parse(input)
}

// t_lookahead: '(' | '[' | '.'
fn t_lookahead(input: &[Token]) -> ParseResult<Token> {
    tok(TT::LPAR)
        .or(tok(TT::LSQB))
        .or(tok(TT::DOT))
        .parse(input)
}

// # Targets for del statements
// # --------------------------

// del_targets: ','.del_target+ [',']
fn del_targets(input: &[Token]) -> ParseResult<Expression> {
    todo!()
}

// del_target:
//     | t_primary '.' NAME !t_lookahead
//     | t_primary '[' slices ']' !t_lookahead
//     | del_t_atom
fn del_target(input: &[Token]) -> ParseResult<Expression> {
    todo!()
}

// del_t_atom:
//     | NAME
//     | '(' del_target ')'
//     | '(' [del_targets] ')'
//     | '[' [del_targets] ']'
fn del_t_atom(input: &[Token]) -> ParseResult<Expression> {
    todo!()
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
//     | TYPE_COMMENT

// # ========================= END OF THE GRAMMAR ===========================

// # ========================= START OF INVALID RULES =======================
