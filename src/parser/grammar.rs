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

use super::ast::*;
use super::combinators::*;
use super::tokenizer::{Token, TokenType as TT};

pub fn parse(input: &[Token]) -> (Vec<Statement>, Vec<Error>) {
    let errors = RefCell::new(Vec::new());
    let input = ParserInput::new(input, ParserState::new(&errors));
    let stmts = file_.parse(input).expect("Parser shall not fail.");
    (stmts, errors.into_inner())
}

pub fn parse_interactive(input: &[Token]) -> (Vec<Statement>, Vec<Error>) {
    let errors = RefCell::new(Vec::new());
    let input = ParserInput::new(input, ParserState::new(&errors));
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
fn eval(input: ParserInput) -> ParseResult<Vec<Expression>> {
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
    token(TT::KEYWORD, "pass")
        .map(|_| Statement::Pass)
        .parse(input)
}

fn break_(input: ParserInput) -> ParseResult<Statement> {
    token(TT::KEYWORD, "break")
        .map(|_| Statement::Break)
        .parse(input)
}

fn continue_(input: ParserInput) -> ParseResult<Statement> {
    token(TT::KEYWORD, "continue")
        .map(|_| Statement::Continue)
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
        .or(star_expressions.map(Statement::Expressions))
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
    function_def
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
    .or(pair(
        pair(
            one_or_more(left(star_targets, tok(TT::EQUAL))),
            yield_expr.map(|e| vec![e]).or(star_expressions),
        ),
        right(not(tok(TT::EQUAL)), maybe(tok(TT::TYPE_COMMENT))),
    )
    .map(|((l, r), t)| {
        Statement::Assignment(
            l.concat(),
            None,
            Some(r),
            t.map(|s| Box::new(Expression::TypeComment(s.lexeme))),
        )
    }))
    .or(pair(
        pair(single_target, augassign),
        yield_expr.map(|e| vec![e]).or(star_expressions),
    )
    .map(|((l, o), r)| Statement::Assignment(vec![l], Some(o.into()), Some(r), None)))
    .parse(input)
}

// annotated_rhs: yield_expr | star_expressions
fn annotated_rhs(input: ParserInput) -> ParseResult<Vec<Expression>> {
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
    right(token(TT::KEYWORD, "return"), star_expressions)
        .map(Statement::Return)
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
    .map(|(e, f)| Statement::Raise(Some(Box::new(e)), f.map(Box::new)))
    .or(token(TT::KEYWORD, "raise").map(|_| Statement::Raise(None, None)))
    .parse(input)
}

// global_stmt: 'global' ','.NAME+
fn global_stmt(input: ParserInput) -> ParseResult<Statement> {
    right(token(TT::KEYWORD, "global"), sep_by(name, TT::COMMA))
        .map(Statement::Global)
        .parse(input)
}

// nonlocal_stmt: 'nonlocal' ','.NAME+
fn nonlocal_stmt(input: ParserInput) -> ParseResult<Statement> {
    right(token(TT::KEYWORD, "nonlocal"), sep_by(name, TT::COMMA))
        .map(Statement::Nonlocal)
        .parse(input)
}

// del_stmt:
//     | 'del' del_targets &(';' | NEWLINE)
fn del_stmt(input: ParserInput) -> ParseResult<Statement> {
    left(
        right(token(TT::KEYWORD, "del"), del_targets),
        lookahead(tok(TT::SEMI).or(tok(TT::NEWLINE))),
    )
    .map(Statement::Del)
    .parse(input)
}

// yield_stmt: yield_expr
fn yield_stmt(input: ParserInput) -> ParseResult<Statement> {
    yield_expr
        .map(|e| Statement::Yield(Box::new(e)))
        .parse(input)
}

// assert_stmt: 'assert' expression [',' expression ]
fn assert_stmt(input: ParserInput) -> ParseResult<Statement> {
    right(
        token(TT::KEYWORD, "assert"),
        pair(expression, maybe(right(tok(TT::COMMA), expression))),
    )
    .map(|(e, m)| Statement::Assert(Box::new(e), m.map(Box::new)))
    .parse(input)
}

// import_stmt:
//     | import_name
//     | import_from
fn import_stmt(input: ParserInput) -> ParseResult<Statement> {
    import_name
        .or(import_from.map(|t| vec![t]))
        .map(Statement::Import)
        .parse(input)
}

// # Import statements
// # -----------------

// import_name: 'import' dotted_as_names
fn import_name(input: ParserInput) -> ParseResult<Vec<Import>> {
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
fn import_from(input: ParserInput) -> ParseResult<Import> {
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
        module: Module {
            rel_level,
            path,
            alias: None,
        },
        items,
    })
    .parse(input)
}

// import_from_targets:
//     | '(' import_from_as_names [','] ')'
//     | import_from_as_names !','
//     | '*'
fn import_from_targets(input: ParserInput) -> ParseResult<Vec<ImportItem>> {
    left(
        right(tok(TT::LPAR), import_from_as_names),
        pair(maybe(tok(TT::COMMA)), tok(TT::RPAR)),
    )
    .or(left(import_from_as_names, not(tok(TT::COMMA))))
    .or(tok(TT::STAR).map(|_| {
        vec![ImportItem {
            name: vec![],
            alias: None,
        }]
    }))
    .parse(input)
}

// import_from_as_names:
//     | ','.import_from_as_name+
fn import_from_as_names(input: ParserInput) -> ParseResult<Vec<ImportItem>> {
    sep_by(import_from_as_name, TT::COMMA).parse(input)
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
        .map(|(d, mut c)| {
            c.decorators = d;
            c
        })
        .map(Statement::ClassDefinition)
        .parse(input)
}

// class_def_raw:
//     | 'class' NAME [type_params] ['(' [arguments] ')' ] ':' block
fn class_def_raw(input: ParserInput) -> ParseResult<ClassDefinition> {
    pair(
        right(token(TT::KEYWORD, "class"), pair(name, maybe(type_params))),
        pair(
            maybe(right(tok(TT::LPAR), left(arguments, tok(TT::RPAR)))),
            right(tok(TT::COLON), block),
        ),
    )
    .map(|((name, ts), (ancestors, body))| ClassDefinition {
        name,
        ancestors: ancestors.unwrap_or(Arguments::empty()),
        body,
        decorators: vec![],
        type_params: ts.unwrap_or_default(),
    })
    .parse(input)
}

// # Function definitions
// # --------------------

// function_def:
//     | decorators function_def_raw
fn function_def(input: ParserInput) -> ParseResult<Statement> {
    pair(decorators, function_def_raw)
        .map(|(dec, fun)| Statement::FunctionDeclaration(fun, dec))
        .parse(input)
}

// function_def_raw:
//     | 'def' NAME [type_params] '(' [params] ')' ['->' expression ] ':' [func_type_comment] block
//     | ASYNC 'def' NAME [type_params] '(' [params] ')' ['->' expression ] ':' [func_type_comment] block
fn function_def_raw(input: ParserInput) -> ParseResult<FunctionDeclaration> {
    pair(
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
    .map(|(a, ((n, p), b))| FunctionDeclaration {
        name: n,
        parameters: p,
        code: b,
        is_async: a.is_some(),
    })
    .parse(input)
}

// # Function parameters
// # -------------------
fn name(input: ParserInput) -> ParseResult<Name> {
    tok(TT::NAME).parse(input).into()
}

// params:
//     | parameters
fn params(input: ParserInput) -> ParseResult<Vec<Parameter>> {
    parameters.parse(input)
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
fn kwds(input: ParserInput) -> ParseResult<Parameter> {
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
        p.type_comment = t.map(|s| s.lexeme);
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
        p.type_comment = t.map(|s| s.lexeme);
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
fn annotation(input: ParserInput) -> ParseResult<Expression> {
    right(tok(TT::COLON), expression).parse(input)
}

// star_annotation: ':' star_expression
fn star_annotation(input: ParserInput) -> ParseResult<Expression> {
    right(tok(TT::COLON), star_expression).parse(input)
}

// default: '=' expression  | invalid_default
fn default(input: ParserInput) -> ParseResult<Expression> {
    right(tok(TT::EQUAL), expression).parse(input)
}

// # If statement
// # ------------

// if_stmt:
//     | 'if' named_expression ':' block elif_stmt
//     | 'if' named_expression ':' block [else_block]
fn if_stmt(input: ParserInput) -> ParseResult<Statement> {
    pair(
        pair(
            right(token(TT::KEYWORD, "if"), named_expression),
            right(tok(TT::COLON), block),
        ),
        elif_stmt.or(maybe(else_block).map(|e| (vec![], e))),
    )
    .map(|((expr, then), (elif, els))| Statement::If(expr, then, elif, els))
    .parse(input)
}

// elif_stmt:
//     | 'elif' named_expression ':' block elif_stmt
//     | 'elif' named_expression ':' block [else_block]
fn elif_stmt(
    input: ParserInput,
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
fn else_block(input: ParserInput) -> ParseResult<Vec<Statement>> {
    right(pair(token(TT::KEYWORD, "else"), tok(TT::COLON)), block).parse(input)
}

// # While statement
// # ---------------

// while_stmt:
//     | 'while' named_expression ':' block [else_block]
fn while_stmt(input: ParserInput) -> ParseResult<Statement> {
    pair(
        right(
            token(TT::KEYWORD, "while"),
            pair(left(named_expression, tok(TT::COLON)), block),
        ),
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
fn for_stmt(input: ParserInput) -> ParseResult<Statement> {
    pair(
        maybe(token(TT::KEYWORD, "async")),
        pair(
            left(
                right(token(TT::KEYWORD, "for"), star_targets),
                token(TT::KEYWORD, "in"),
            ),
            pair(
                left(star_expressions, tok(TT::COLON)),
                pair(pair(maybe(tok(TT::TYPE_COMMENT)), block), maybe(else_block)),
            ),
        ),
    )
    .map(|(a, (tgt, (expr, ((tc, blck), els))))| {
        Statement::For(
            tgt,
            expr,
            blck,
            els,
            tc.map(|s| Expression::TypeComment(s.lexeme)),
            a.is_some(),
        )
    })
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
    pair(
        maybe(token(TT::KEYWORD, "async")),
        pair(
            right(
                pair(token(TT::KEYWORD, "with"), tok(TT::LPAR)),
                left(sep_by(with_item, TT::COMMA), maybe(tok(TT::COMMA))),
            ),
            right(pair(tok(TT::RPAR), tok(TT::COLON)), block),
        ),
    )
    .map(|(a, (w, b))| (a, (w, (None, b))))
    .or(pair(
        maybe(token(TT::KEYWORD, "async")),
        right(
            pair(token(TT::KEYWORD, "with"), tok(TT::COLON)),
            pair(
                sep_by(with_item, TT::COMMA),
                right(tok(TT::COLON), pair(maybe(tok(TT::TYPE_COMMENT)), block)),
            ),
        ),
    ))
    .map(|(a, (w, (t, b)))| {
        Statement::With(
            w,
            b,
            t.map(|s| Expression::TypeComment(s.lexeme)),
            a.is_some(),
        )
    })
    .parse(input)
}

// with_item:
//     | expression ['as' star_target &(',' | ')' | ':')]
fn with_item(input: ParserInput) -> ParseResult<Expression> {
    pair(
        expression,
        maybe(left(
            right(token(TT::KEYWORD, "as"), star_target),
            lookahead(tok(TT::COMMA).or(tok(TT::RPAR)).or(tok(TT::COLON))),
        )),
    )
    .map(|(e, t)| Expression::WithItem(Box::new(e), t.map(Box::new)))
    .parse(input)
}

// # Try statement
// # -------------

// try_stmt:
//     | 'try' ':' block finally_block
//     | 'try' ':' block except_block+ [else_block] [finally_block]
//     | 'try' ':' block except_star_block+ [else_block] [finally_block]
fn try_stmt(input: ParserInput) -> ParseResult<Statement> {
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
fn except_block(input: ParserInput) -> ParseResult<Expression> {
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
fn except_star_block(input: ParserInput) -> ParseResult<Expression> {
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
fn finally_block(input: ParserInput) -> ParseResult<Vec<Statement>> {
    right(pair(token(TT::KEYWORD, "finally"), tok(TT::COLON)), block).parse(input)
}

// # Match statement
// # ---------------

// match_stmt:
//     | "match" subject_expr ':' NEWLINE INDENT case_block+ DEDENT
fn match_stmt(input: ParserInput) -> ParseResult<Statement> {
    pair(
        right(token(TT::SOFT_KEYWORD, "match"), subject_expr),
        right(
            pair(pair(tok(TT::COLON), tok(TT::NEWLINE)), tok(TT::INDENT)),
            left(one_or_more(case_block), tok(TT::DEDENT)),
        ),
    )
    .map(|(s, cs)| Statement::Match(s, cs))
    .parse(input)
}

// subject_expr:
//     | star_named_expression ',' star_named_expressions?
//     | named_expression
fn subject_expr(input: ParserInput) -> ParseResult<Vec<Expression>> {
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
fn case_block(input: ParserInput) -> ParseResult<Expression> {
    pair(
        right(token(TT::SOFT_KEYWORD, "case"), patterns),
        pair(left(maybe(guard), tok(TT::COLON)), block),
    )
    .map(|(p, (g, b))| Expression::Case(p, g.map(Box::new), b))
    .parse(input)
}

// guard: 'if' named_expression
fn guard(input: ParserInput) -> ParseResult<Expression> {
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
    .map(|(p, t)| Pattern::Capture(Some(Box::new(p)), t))
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
        .map(Expression::Number)
        .or(strings)
        .or(token(TT::KEYWORD, "None").map(|_| Expression::None))
        .or(token(TT::KEYWORD, "True").map(|_| Expression::True))
        .or(token(TT::KEYWORD, "False").map(|_| Expression::False))
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
        .map(|p| Expression::Pattern(Box::new(p)))
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
            Expression::Attribute(ns) => Pattern::Value(ns),
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
            Expression::Attribute(ns)
        })
        .parse(input)
}

fn name_or_attr(input: ParserInput) -> ParseResult<Expression> {
    name.map(|n| Expression::Attribute(vec![n]))
        .or(attr)
        .parse(input)
}

// group_pattern:
//     | '(' pattern ')'
fn group_pattern(input: ParserInput) -> ParseResult<Pattern> {
    right(tok(TT::LPAR), left(pattern, tok(TT::RPAR)))
        .map(|p| Pattern::Group(Box::new(p)))
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
    .map(|p| Pattern::Star(Box::new(p)))
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
        .map(|(e, p)| Pattern::KeyValue(Box::new(e), Box::new(p)))
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
    .map(|(n, ps)| Pattern::Class(Box::new(n), ps))
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
        .map(|(n, p)| Pattern::KeyValue(Box::new(Expression::Name(n)), Box::new(p)))
        .parse(input)
}

// # Type statement
// # ---------------

// type_alias:
//     | "type" NAME [type_params] '=' expression

fn type_alias(input: ParserInput) -> ParseResult<Statement> {
    pair(
        right(token(TT::SOFT_KEYWORD, "type"), name),
        pair(left(maybe(type_params), tok(TT::EQUAL)), expression),
    )
    .map(|(n, (t, e))| Statement::Type(n, t.unwrap_or_default(), Box::new(e)))
    .parse(input)
}

// # Type parameter declaration
// # --------------------------

// type_params: '[' type_param_seq  ']'
fn type_params(input: ParserInput) -> ParseResult<Vec<Expression>> {
    left(right(tok(TT::LSQB), type_param_seq), tok(TT::RSQB)).parse(input)
}

// type_param_seq: ','.type_param+ [',']
fn type_param_seq(input: ParserInput) -> ParseResult<Vec<Expression>> {
    left(sep_by(type_param, TT::COMMA), maybe(tok(TT::COMMA))).parse(input)
}

// type_param:
//     | NAME [type_param_bound]
//     | '*' NAME ':' expression
//     | '*' NAME
//     | '**' NAME ':' expression
//     | '**' NAME
fn type_param(input: ParserInput) -> ParseResult<Expression> {
    pair(name, maybe(type_param_bound))
        .map(|(name, type_bound)| {
            Expression::TypeBound(TypeBound {
                name,
                type_bound,
                starred: false,
                double_starred: false,
            })
        })
        .or(pair(
            right(tok(TT::STAR), name),
            right(tok(TT::COLON), expression),
        )
        .map(|(name, t)| {
            Expression::TypeBound(TypeBound {
                name,
                type_bound: Some(Box::new(t)),
                starred: true,
                double_starred: false,
            })
        }))
        .or(right(tok(TT::STAR), name).map(|name| {
            Expression::TypeBound(TypeBound {
                name,
                type_bound: None,
                starred: true,
                double_starred: false,
            })
        }))
        .or(pair(
            right(tok(TT::DOUBLESTAR), name),
            right(tok(TT::COLON), expression),
        )
        .map(|(name, t)| {
            Expression::TypeBound(TypeBound {
                name,
                type_bound: Some(Box::new(t)),
                starred: false,
                double_starred: true,
            })
        }))
        .or(right(tok(TT::DOUBLESTAR), name).map(|name| {
            Expression::TypeBound(TypeBound {
                name,
                type_bound: None,
                starred: false,
                double_starred: true,
            })
        }))
        .parse(input)
}

// type_param_bound: ':' expression
fn type_param_bound(input: ParserInput) -> ParseResult<Box<Expression>> {
    right(tok(TT::COLON), expression).map(Box::new).parse(input)
}

// # EXPRESSIONS
// # -----------

// expressions:
//     | expression (',' expression )+ [',']
//     | expression ','
//     | expression
fn expressions(input: ParserInput) -> ParseResult<Vec<Expression>> {
    left(sep_by(expression, TT::COMMA), maybe(tok(TT::COMMA))).parse(input)
}

// expression:
//     | disjunction 'if' disjunction 'else' expression
//     | disjunction
//     | lambdef
fn expression(input: ParserInput) -> ParseResult<Expression> {
    pair(
        disjunction,
        maybe(pair(
            right(token(TT::KEYWORD, "if"), disjunction),
            right(token(TT::KEYWORD, "else"), expression),
        )),
    )
    .map(|(t, o)| match o {
        Some((c, e)) => Expression::Ternary(Box::new(c), Box::new(t), Box::new(e)),
        None => t,
    })
    .or(lambdef)
    .parse(input)
}

// yield_expr:
//     | 'yield' 'from' expression
//     | 'yield' [star_expressions]
fn yield_expr(input: ParserInput) -> ParseResult<Expression> {
    right(
        pair(token(TT::KEYWORD, "yield"), token(TT::KEYWORD, "from")),
        expression,
    )
    .map(|e| Expression::YieldFrom(Box::new(e)))
    .or(right(
        token(TT::KEYWORD, "yield"),
        maybe(star_expressions).map(|v| v.unwrap_or_default()),
    )
    .map(Expression::Yield))
    .parse(input)
}

// star_expressions:
//     | star_expression (',' star_expression )+ [',']
//     | star_expression ','
//     | star_expression
fn star_expressions(input: ParserInput) -> ParseResult<Vec<Expression>> {
    left(sep_by(star_expression, TT::COMMA), maybe(tok(TT::COMMA))).parse(input)
}

// star_expression:
//     | '*' bitwise_or
//     | expression
fn star_expression(input: ParserInput) -> ParseResult<Expression> {
    pair(tok(TT::STAR), bitwise_or)
        .map(|(_, e)| Expression::ListUnwrap(Box::new(e)))
        .or(expression)
        .parse(input)
}

// star_named_expressions: ','.star_named_expression+ [',']
fn star_named_expressions(input: ParserInput) -> ParseResult<Vec<Expression>> {
    left(
        sep_by(star_named_expression, TT::COMMA),
        maybe(tok(TT::COMMA)),
    )
    .parse(input)
}

// star_named_expression:
//     | '*' bitwise_or
//     | named_expression
fn star_named_expression(input: ParserInput) -> ParseResult<Expression> {
    right(tok(TT::STAR), bitwise_or)
        .or(named_expression)
        .parse(input)
}

// assignment_expression:
//     | NAME ':=' ~ expression
fn assignment_expression(input: ParserInput) -> ParseResult<Expression> {
    pair(
        left(name.map(Expression::Name), tok(TT::COLONEQUAL)),
        expression,
    )
    .map(|(l, r)| Expression::Walrus(Box::new(l), Box::new(r)))
    .parse(input)
}

// named_expression:
//     | assignment_expression
//     | expression !':='
fn named_expression(input: ParserInput) -> ParseResult<Expression> {
    assignment_expression
        .or(left(expression, not(tok(TT::COLONEQUAL))))
        .parse(input)
}

// disjunction:
//     | conjunction ('or' conjunction )+
//     | conjunction
fn disjunction(input: ParserInput) -> ParseResult<Expression> {
    pair(conjunction, right(token(TT::KEYWORD, "or"), conjunction))
        .map(|(l, r)| Expression::BinaryOperation(Operator::Or, Box::new((l, r))))
        .or(conjunction)
        .parse(input)
}

// conjunction:
//     | inversion ('and' inversion )+
//     | inversion
fn conjunction(input: ParserInput) -> ParseResult<Expression> {
    pair(inversion, right(token(TT::KEYWORD, "and"), inversion))
        .map(|(l, r)| Expression::BinaryOperation(Operator::And, Box::new((l, r))))
        .or(inversion)
        .parse(input)
}

// inversion:
//     | 'not' inversion
//     | comparison
fn inversion(input: ParserInput) -> ParseResult<Expression> {
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
fn comparison(input: ParserInput) -> ParseResult<Expression> {
    pair(bitwise_or, zero_or_more(compare_op_bitwise_or_pair))
        .map(|(l, r)| {
            if r.is_empty() {
                l
            } else {
                Expression::Comparison(Box::new(l), r)
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
fn compare_op_bitwise_or_pair(input: ParserInput) -> ParseResult<(Operator, Expression)> {
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
fn eq_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Expression)> {
    pair(tok(TT::EQEQUAL).map(Operator::from), bitwise_or).parse(input)
}

// noteq_bitwise_or:
//     | ('!=' ) bitwise_or
fn noteq_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Expression)> {
    pair(tok(TT::NOTEQUAL).map(Operator::from), bitwise_or).parse(input)
}

// lte_bitwise_or: '<=' bitwise_or
fn lte_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Expression)> {
    pair(tok(TT::LESSEQUAL).map(Operator::from), bitwise_or).parse(input)
}

// lt_bitwise_or: '<' bitwise_or
fn lt_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Expression)> {
    pair(tok(TT::LESS).map(Operator::from), bitwise_or).parse(input)
}

// gte_bitwise_or: '>=' bitwise_or
fn gte_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Expression)> {
    pair(tok(TT::GREATEREQUAL).map(Operator::from), bitwise_or).parse(input)
}

// gt_bitwise_or: '>' bitwise_or
fn gt_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Expression)> {
    pair(tok(TT::GREATER).map(Operator::from), bitwise_or).parse(input)
}

// notin_bitwise_or: 'not' 'in' bitwise_or
fn notin_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Expression)> {
    right(
        pair(token(TT::KEYWORD, "not"), token(TT::KEYWORD, "in")),
        bitwise_or,
    )
    .map(|e| (Operator::NotIn, e))
    .parse(input)
}

// in_bitwise_or: 'in' bitwise_or
fn in_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Expression)> {
    right(token(TT::KEYWORD, "in"), bitwise_or)
        .map(|e| (Operator::In, e))
        .parse(input)
}

// isnot_bitwise_or: 'is' 'not' bitwise_or
fn isnot_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Expression)> {
    right(
        pair(token(TT::KEYWORD, "is"), token(TT::KEYWORD, "not")),
        bitwise_or,
    )
    .map(|e| (Operator::IsNot, e))
    .parse(input)
}

// is_bitwise_or: 'is' bitwise_or
fn is_bitwise_or(input: ParserInput) -> ParseResult<(Operator, Expression)> {
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
fn bitwise_or(input: ParserInput) -> ParseResult<Expression> {
    pair(bitwise_xor, bitwise_or_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::BinaryOperation(_, expr, tail) => (
                        Expression::BinaryOperation(
                            Operator::BitwiseOr,
                            Box::new((current_expr, *expr)),
                        ),
                        tail,
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
fn bitwise_xor(input: ParserInput) -> ParseResult<Expression> {
    pair(bitwise_and, bitwise_xor_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::BinaryOperation(_, expr, tail) => (
                        Expression::BinaryOperation(
                            Operator::BitwiseXor,
                            Box::new((current_expr, *expr)),
                        ),
                        tail,
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
fn bitwise_and(input: ParserInput) -> ParseResult<Expression> {
    pair(shift_expr, bitwise_and_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::BinaryOperation(_, expr, tail) => (
                        Expression::BinaryOperation(
                            Operator::BitwiseAnd,
                            Box::new((current_expr, *expr)),
                        ),
                        tail,
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
fn shift_expr(input: ParserInput) -> ParseResult<Expression> {
    pair(sum, shift_expr_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::BinaryOperation(op, expr, tail) => (
                        Expression::BinaryOperation(op, Box::new((current_expr, *expr))),
                        tail,
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
fn sum(input: ParserInput) -> ParseResult<Expression> {
    pair(term, sum_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::BinaryOperation(op, expr, tail) => (
                        Expression::BinaryOperation(op, Box::new((current_expr, *expr))),
                        tail,
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
fn term(input: ParserInput) -> ParseResult<Expression> {
    pair(factor, term_tail)
        .map(|(a, tail)| {
            let mut current_expr = a;
            let mut new_tail = Box::new(tail);
            while !new_tail.empty() {
                (current_expr, new_tail) = match *new_tail {
                    IncompleteExpression::Empty => {
                        (current_expr, Box::new(IncompleteExpression::Empty))
                    }
                    IncompleteExpression::BinaryOperation(op, expr, tail) => (
                        Expression::BinaryOperation(op, Box::new((current_expr, *expr))),
                        tail,
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
fn factor(input: ParserInput) -> ParseResult<Expression> {
    power
        .or(
            pair(tok(TT::PLUS).or(tok(TT::MINUS)).or(tok(TT::TILDE)), factor).map(|(o, e)| {
                let op = o.into();
                match op {
                    Operator::Minus | Operator::BitwiseNot => {
                        Expression::UnaryOperation(op, Box::new(e))
                    }
                    _ => e,
                }
            }),
        )
        .parse(input)
}

// power:
//     | await_primary ['**' factor]
fn power(input: ParserInput) -> ParseResult<Expression> {
    pair(await_primary, maybe(pair(tok(TT::DOUBLESTAR), factor)))
        .map(|(l, exp)| match exp {
            Some((o, r)) => Expression::BinaryOperation(o.into(), Box::new((l, r))),
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
fn await_primary(input: ParserInput) -> ParseResult<Expression> {
    right(token(TT::KEYWORD, "await"), primary)
        .or(primary)
        .parse(input)
}

// primary:
//   | atom primary_tail
fn primary(input: ParserInput) -> ParseResult<Expression> {
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
                        (Expression::Call(Box::new(current_expr), args), tail)
                    }
                    IncompleteExpression::Slice(slice, tail) => {
                        (Expression::Slice(Box::new(current_expr), slice), tail)
                    }
                    IncompleteExpression::Subscript(name, tail) => {
                        (Expression::Subscript(Box::new(current_expr), name), tail)
                    }
                    IncompleteExpression::PrimaryGenexp(genexp, tail) => (
                        Expression::PrimaryGenexp(Box::new(current_expr), genexp),
                        tail,
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
            IncompleteExpression::Call(a.unwrap_or(Arguments::empty()), Box::new(tail))
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
fn atom(input: ParserInput) -> ParseResult<Expression> {
    name.map(Expression::Name)
        .or(token(TT::KEYWORD, "True").map(|_| Expression::True))
        .or(token(TT::KEYWORD, "False").map(|_| Expression::False))
        .or(token(TT::KEYWORD, "None").map(|_| Expression::None))
        .or(strings)
        .or(number.map(Expression::Number))
        .or(tuple.or(group).or(genexp))
        .or(list.or(listcomp))
        .or(dict.or(set).or(dictcomp).or(setcomp))
        .or(tok(TT::ELLIPSIS).map(|_| Expression::Ellipsis))
        .parse(input)
}

fn number(input: ParserInput) -> ParseResult<Number> {
    tok(TT::NUMBER).parse(input).into()
}

// group:
//     | '(' (yield_expr | named_expression) ')'
fn group(input: ParserInput) -> ParseResult<Expression> {
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
fn lambdef(input: ParserInput) -> ParseResult<Expression> {
    pair(
        right(token(TT::KEYWORD, "lambda"), maybe(lambda_parameters)),
        expression,
    )
    .map(|(p, b)| Expression::Lambda(p.unwrap_or_default(), Box::new(b)))
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
fn lambda_parameters(input: ParserInput) -> ParseResult<Vec<Parameter>> {
    pair(
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
        p.extend(q);
        p.extend(r);
        p.extend(s.unwrap_or_default());
        p
    })
    .or(pair(
        pair(
            lambda_slash_with_default,
            zero_or_more(lambda_param_with_default),
        ),
        maybe(lambda_star_etc),
    )
    .map(|((mut p, q), r)| {
        p.extend(q);
        p.extend(r.unwrap_or_default());
        p
    }))
    .or(pair(
        pair(
            one_or_more(lambda_param_no_default),
            zero_or_more(lambda_param_with_default),
        ),
        maybe(lambda_star_etc),
    )
    .map(|((mut p, q), r)| {
        p.extend(q);
        p.extend(r.unwrap_or_default());
        p
    }))
    .or(pair(
        one_or_more(lambda_param_with_default),
        maybe(lambda_star_etc),
    )
    .map(|(mut p, q)| {
        p.extend(q.unwrap_or_default());
        p
    }))
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
fn lambda_star_etc(input: ParserInput) -> ParseResult<Vec<Parameter>> {
    pair(
        right(tok(TT::STAR), lambda_param_no_default),
        pair(zero_or_more(lambda_param_maybe_default), maybe(lambda_kwds)),
    )
    .map(|(mut p, (mut q, r))| {
        p.starred = true;
        q.insert(0, p);
        if let Some(kwds) = r {
            q.push(kwds);
        }
        q
    })
    .or(pair(
        right(
            pair(tok(TT::STAR), tok(TT::COMMA)),
            one_or_more(lambda_param_maybe_default),
        ),
        maybe(lambda_kwds),
    )
    .map(|(mut p, q)| {
        if let Some(kwds) = q {
            p.push(kwds);
        }
        p
    }))
    .or(lambda_kwds.map(|k| vec![k]))
    .parse(input)
}

// lambda_kwds:
//     | '**' lambda_param_no_default
fn lambda_kwds(input: ParserInput) -> ParseResult<Parameter> {
    right(tok(TT::DOUBLESTAR), lambda_param_no_default)
        .map(|mut p| {
            p.double_starred = true;
            p
        })
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
fn fstring_middle(input: ParserInput) -> ParseResult<FString> {
    fstring_replacement_field
        .map(FString::Interpolated)
        .or(tok(TT::FSTRING_MIDDLE).map(|f| FString::Literal(f.lexeme)))
        .parse(input)
}

// fstring_replacement_field:
//     | '{' (yield_expr | star_expressions) '='? [fstring_conversion] [fstring_full_format_spec] '}'
fn fstring_replacement_field(input: ParserInput) -> ParseResult<FStringReplacement> {
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
    .map(|((exprs, (dbg, conversion)), fmt)| FStringReplacement {
        exprs,
        debug: dbg.is_some(),
        conversion,
        format_specs: fmt.unwrap_or_default(),
    })
    .parse(input)
}

// fstring_conversion:
//     | "!" NAME
fn fstring_conversion(input: ParserInput) -> ParseResult<Name> {
    right(tok(TT::EXCLAMATION), name).parse(input)
}

// fstring_full_format_spec:
//     | ':' fstring_format_spec*
fn fstring_full_format_spec(input: ParserInput) -> ParseResult<Vec<Expression>> {
    right(tok(TT::COLON), zero_or_more(fstring_format_spec)).parse(input)
}

// fstring_format_spec:
//     | FSTRING_MIDDLE
//     | fstring_replacement_field
fn fstring_format_spec(input: ParserInput) -> ParseResult<Expression> {
    tok(TT::FSTRING_MIDDLE)
        .map(|t| Expression::Strings(vec![PyString::Literal(t.lexeme)]))
        .or(fstring_replacement_field.map(Expression::FStringReplacement))
        .parse(input)
}

// fstring:
//     | FSTRING_START fstring_middle* FSTRING_END
fn fstring(input: ParserInput) -> ParseResult<PyString> {
    left(
        right(tok(TT::FSTRING_START), zero_or_more(fstring_middle)),
        tok(TT::FSTRING_END),
    )
    .map(PyString::FString)
    .parse(input)
}

// string: STRING
fn string(input: ParserInput) -> ParseResult<Token> {
    tok(TT::STRING).parse(input)
}

// strings: (fstring|string)+
fn strings(input: ParserInput) -> ParseResult<Expression> {
    one_or_more(string.map(|t| PyString::Literal(t.lexeme)).or(fstring))
        .map(Expression::Strings)
        .parse(input)
}

// list:
//     | '[' [star_named_expressions] ']'
fn list(input: ParserInput) -> ParseResult<Expression> {
    left(
        right(tok(TT::LSQB), maybe(star_named_expressions)),
        tok(TT::RSQB),
    )
    .map(|e| Expression::List(e.unwrap_or_default()))
    .parse(input)
}

// tuple:
//     | '(' [star_named_expression ',' [star_named_expressions]  ] ')'
fn tuple(input: ParserInput) -> ParseResult<Expression> {
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
            v.extend(es.unwrap_or_default());
        }
        Expression::Tuple(v)
    })
    .parse(input)
}

// set: '{' star_named_expressions '}'
fn set(input: ParserInput) -> ParseResult<Expression> {
    left(
        right(tok(TT::LBRACE), star_named_expressions),
        tok(TT::RBRACE),
    )
    .map(Expression::Set)
    .parse(input)
}

// # Dicts
// # -----

// dict:
//     | '{' [double_starred_kvpairs] '}'
fn dict(input: ParserInput) -> ParseResult<Expression> {
    left(
        right(tok(TT::LBRACE), maybe(double_starred_kvpairs)),
        tok(TT::RBRACE),
    )
    .map(|e| Expression::Dict(e.unwrap_or_default()))
    .parse(input)
}

// double_starred_kvpairs: ','.double_starred_kvpair+ [',']
fn double_starred_kvpairs(input: ParserInput) -> ParseResult<Vec<Expression>> {
    left(
        sep_by(double_starred_kvpair, TT::COMMA),
        maybe(tok(TT::COMMA)),
    )
    .parse(input)
}

// double_starred_kvpair:
//     | '**' bitwise_or
//     | kvpair
fn double_starred_kvpair(input: ParserInput) -> ParseResult<Expression> {
    right(tok(TT::DOUBLESTAR), bitwise_or)
        .map(|e| Expression::DictUnwrap(Box::new(e)))
        .or(kvpair)
        .parse(input)
}

// kvpair: expression ':' expression
fn kvpair(input: ParserInput) -> ParseResult<Expression> {
    pair(expression, right(tok(TT::COLON), expression))
        .map(|(e, f)| Expression::Tuple(vec![e, f]))
        .parse(input)
}

// # Comprehensions & Generators
// # ---------------------------

// for_if_clauses:
//     | for_if_clause+
fn for_if_clauses(input: ParserInput) -> ParseResult<Vec<Expression>> {
    one_or_more(for_if_clause).parse(input)
}

// for_if_clause:
//     | ASYNC 'for' star_targets 'in' ~ disjunction ('if' disjunction )*
//     | 'for' star_targets 'in' ~ disjunction ('if' disjunction )*
fn for_if_clause(input: ParserInput) -> ParseResult<Expression> {
    pair(
        maybe(token(TT::KEYWORD, "async")),
        pair(
            pair(
                right(token(TT::KEYWORD, "for"), star_targets),
                right(token(TT::KEYWORD, "in"), disjunction),
            ),
            zero_or_more(right(token(TT::KEYWORD, "if"), disjunction)),
        ),
    )
    .map(|(a, ((tgt, set), ifs))| Expression::ForIfClause(tgt, Box::new(set), ifs, a.is_some()))
    .parse(input)
}

// listcomp:
//     | '[' named_expression for_if_clauses ']'
fn listcomp(input: ParserInput) -> ParseResult<Expression> {
    pair(
        right(tok(TT::LSQB), star_named_expression),
        left(for_if_clauses, tok(TT::RSQB)),
    )
    .map(|(e, f)| Expression::ListComprehension(Box::new(e), f))
    .parse(input)
}

// setcomp:
//     | '{' named_expression for_if_clauses '}'
fn setcomp(input: ParserInput) -> ParseResult<Expression> {
    pair(
        right(tok(TT::LBRACE), named_expression),
        left(for_if_clauses, tok(TT::RBRACE)),
    )
    .map(|(e, f)| Expression::SetComprehension(Box::new(e), f))
    .parse(input)
}

// genexp:
//     | '(' ( assignment_expression | expression !':=') for_if_clauses ')'
fn genexp(input: ParserInput) -> ParseResult<Expression> {
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
fn dictcomp(input: ParserInput) -> ParseResult<Expression> {
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
fn arguments(input: ParserInput) -> ParseResult<Arguments> {
    left(args, pair(maybe(tok(TT::COMMA)), lookahead(tok(TT::RPAR)))).parse(input)
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
    .parse(input)
}

// kwargs:
//     | ','.kwarg_or_starred+ ',' ','.kwarg_or_double_starred+
//     | ','.kwarg_or_starred+
//     | ','.kwarg_or_double_starred+
fn kwargs(input: ParserInput) -> ParseResult<Vec<Expression>> {
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
fn starred_expression(input: ParserInput) -> ParseResult<Expression> {
    right(tok(TT::STAR), expression)
        .map(|e| Expression::ListUnwrap(Box::new(e)))
        .parse(input)
}

// kwarg_or_starred:
//     | NAME '=' expression
//     | starred_expression
fn kwarg_or_starred(input: ParserInput) -> ParseResult<Expression> {
    pair(left(name, tok(TT::EQUAL)), expression)
        .map(|(n, e)| Expression::KeywordArgument(n, Box::new(e)))
        .or(starred_expression)
        .parse(input)
}

// kwarg_or_double_starred:
//     | NAME '=' expression
//     | '**' expression
fn kwarg_or_double_starred(input: ParserInput) -> ParseResult<Expression> {
    pair(left(name, tok(TT::EQUAL)), expression)
        .map(|(n, e)| Expression::KeywordArgument(n, Box::new(e)))
        .or(right(tok(TT::DOUBLESTAR), expression))
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
fn star_targets(input: ParserInput) -> ParseResult<Vec<Expression>> {
    left(sep_by(star_target, TT::COMMA), maybe(tok(TT::COMMA))).parse(input)
}

// star_targets_list_seq: ','.star_target+ [',']
fn star_targets_list_seq(input: ParserInput) -> ParseResult<Vec<Expression>> {
    left(sep_by(star_target, TT::COMMA), maybe(tok(TT::COMMA))).parse(input)
}

// star_targets_tuple_seq:
//     | star_target (',' star_target )+ [',']
//     | star_target ','
fn star_targets_tuple_seq(input: ParserInput) -> ParseResult<Vec<Expression>> {
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
fn star_target(input: ParserInput) -> ParseResult<Expression> {
    right(tok(TT::STAR), right(not(tok(TT::STAR)), star_target))
        .or(target_with_star_atom)
        .parse(input)
}

// target_with_star_atom:
//     | t_primary '.' NAME !t_lookahead
//     | t_primary '[' slices ']' !t_lookahead
//     | star_atom
fn target_with_star_atom(input: ParserInput) -> ParseResult<Expression> {
    pair(
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
    .parse(input)
}

// star_atom:
//     | NAME
//     | '(' target_with_star_atom ')'
//     | '(' [star_targets_tuple_seq] ')'
//     | '[' [star_targets_list_seq] ']'
fn star_atom(input: ParserInput) -> ParseResult<Expression> {
    name.map(Expression::Name)
        .or(left(
            right(tok(TT::LPAR), target_with_star_atom),
            tok(TT::RPAR),
        ))
        .or(left(
            right(tok(TT::LPAR), maybe(star_targets_tuple_seq)),
            tok(TT::RPAR),
        )
        .map(|v| Expression::Tuple(v.unwrap_or_default())))
        .or(left(
            right(tok(TT::LSQB), maybe(star_targets_list_seq)),
            tok(TT::RSQB),
        )
        .map(|v| Expression::List(v.unwrap_or_default())))
        .parse(input)
}

// single_target:
//     | single_subscript_attribute_target
//     | NAME
//     | '(' single_target ')'
fn single_target(input: ParserInput) -> ParseResult<Expression> {
    single_subscript_attribute_target
        .or(name.map(Expression::Name))
        .or(left(right(tok(TT::LPAR), single_target), tok(TT::RPAR)))
        .parse(input)
}

enum Selector {
    Name(Name),
    Slice(Vec<Slice>),
}

impl Selector {
    fn apply_to(self, expr: Expression) -> Expression {
        let e = Box::new(expr);
        match self {
            Self::Name(n) => Expression::Subscript(e, n),
            Self::Slice(s) => Expression::Slice(e, s),
        }
    }
}

// single_subscript_attribute_target:
//     | t_primary '.' NAME !t_lookahead
//     | t_primary '[' slices ']' !t_lookahead
fn single_subscript_attribute_target(input: ParserInput) -> ParseResult<Expression> {
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
fn t_primary(input: ParserInput) -> ParseResult<Expression> {
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
                        (Expression::Call(Box::new(current_expr), args), tail)
                    }
                    IncompleteExpression::Slice(slice, tail) => {
                        (Expression::Slice(Box::new(current_expr), slice), tail)
                    }
                    IncompleteExpression::Subscript(name, tail) => {
                        (Expression::Subscript(Box::new(current_expr), name), tail)
                    }
                    IncompleteExpression::PrimaryGenexp(genexp, tail) => (
                        Expression::PrimaryGenexp(Box::new(current_expr), genexp),
                        tail,
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
    .map(|(n, tail)| IncompleteExpression::Call(n, Box::new(tail))))
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
fn del_targets(input: ParserInput) -> ParseResult<Vec<Expression>> {
    left(sep_by(del_target, TT::COMMA), maybe(tok(TT::COMMA))).parse(input)
}

// del_target:
//     | t_primary !t_lookahead
//     | del_t_atom
fn del_target(input: ParserInput) -> ParseResult<Expression> {
    left(t_primary, not(t_lookahead))
        .or(del_t_atom)
        .parse(input)
}

// del_t_atom:
//     | NAME
//     | '(' del_target ')'
//     | '(' [del_targets] ')'
//     | '[' [del_targets] ']'
fn del_t_atom(input: ParserInput) -> ParseResult<Expression> {
    name.map(Expression::Name)
        .or(left(right(tok(TT::LPAR), del_target), tok(TT::RPAR)))
        .or(
            left(right(tok(TT::LPAR), maybe(del_targets)), tok(TT::RPAR))
                .map(|v| Expression::Tuple(v.unwrap_or_default())),
        )
        .or(
            left(right(tok(TT::LSQB), maybe(del_targets)), tok(TT::RSQB))
                .map(|v| Expression::List(v.unwrap_or_default())),
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
//     | TYPE_COMMENT

// # ========================= END OF THE GRAMMAR ===========================

// # ========================= START OF INVALID RULES =======================
