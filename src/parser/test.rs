use super::{error::Error, parse, tokenize_string, Block};

fn parse_string(input: &str) -> Result<(Block, Vec<Error>), String> {
    let tokens = tokenize_string(input)?;
    let (tree, errors) = parse(&tokens);
    Ok((tree, errors))
}

fn parse_tree_matches(input: &str, tree_repr: &str) {
    let result = parse_string(input);
    assert!(matches!(result, Ok((_, ref errors)) if errors.is_empty()));
    if let Ok((ref tree, _)) = result {
        let result_repr = format!("{tree:?}");
        assert!(
            result_repr.contains(tree_repr),
            "\nFailed to parse \"{}\":\nexpected \"{}\" somewhere in \"{}\"\n",
            input,
            tree_repr,
            result_repr
        )
    } else {
        unreachable!()
    }
}

fn assert_raises_error(input: &str, msg: &str) {
    let result = parse_string(input);
    assert!(matches!(result, Ok((_, ref errors)) if !errors.is_empty()));
    match result {
        Ok((_, ref errors)) => {
            let err = errors.first().unwrap();
            assert_eq!(msg, err.1.as_str());
        }
        _ => unreachable!(),
    }
}

#[test]
fn test_yield_statement() {
    parse_tree_matches("def f(): yield 1", "Yield");
    parse_tree_matches("def f(): yield", "Yield");
    parse_tree_matches("def f(): x += yield", "Yield");
    parse_tree_matches("def f(): x = yield 1", "Yield");
    parse_tree_matches("def f(): x = y = yield 1", "Yield");
    parse_tree_matches("def f(): x = yield", "Yield");
    parse_tree_matches("def f(): x = y = yield", "Yield");
    parse_tree_matches("def f(): 1 + (yield)*2", "Yield");
    parse_tree_matches("def f(): (yield 1)*2", "Yield");
    parse_tree_matches("def f(): return; yield 1", "Yield");
    parse_tree_matches("def f(): yield 1; return", "Yield");
    parse_tree_matches("def f(): yield from 1", "Yield");
    parse_tree_matches("def f(): x = yield from 1", "Yield");
    parse_tree_matches("def f(): f((yield from 1))", "Yield");
    parse_tree_matches("def f(): yield 1; return 1", "Yield");
    parse_tree_matches(
        "def f():\n    for x in range(30):\n        yield x\n",
        "Yield",
    );
    parse_tree_matches("def f():\n    if (yield):\n        yield x\n", "Yield");
}

#[test]
fn test_await_statement() {
    parse_tree_matches("async def f():\n await smth()", "is_async: true");
    parse_tree_matches("async def f():\n foo = await smth()", "is_async: true");
    parse_tree_matches("async def f():\n foo, bar = await smth()", "is_async: true");
    parse_tree_matches("async def f():\n (await smth())", "is_async: true");
    parse_tree_matches("async def f():\n foo((await smth()))", "is_async: true");
    parse_tree_matches("async def f():\n await foo(); return 42", "is_async: true");
}

#[test]
fn test_async_with_statement() {
    parse_tree_matches("async def f():\n async with 1: pass", "is_async: true");
    parse_tree_matches(
        "async def f():\n async with a as b, c as d: pass",
        "is_async: true",
    );
}

#[test]
fn test_generator_expr_argument() {
    parse_tree_matches("print(foo for foo in foobar)", "Expressions([Call(Name(\"print\"");
    parse_tree_matches("print(foo for foo in foobar)", "Arguments { positional: [Generator(Name(\"foo\"");
    parse_tree_matches("foo(x for x in range(10))", "Call(Name(\"foo\"");
    parse_tree_matches("foo(x for x in range(10))", "Generator(Name(\"x\"");
}

#[test]
fn test_async_for_statement() {
    parse_tree_matches("async def f():\n async for i in (): pass", "is_async: true");
    parse_tree_matches(
        "async def f():\n async for i, b in (): pass",
        "is_async: true",
    );
}

#[test]
fn test_nonlocal_statement() {
    parse_tree_matches(
        "def f():\n    x = 0\n    def g():\n        nonlocal x\n",
        "Nonlocal",
    );
    parse_tree_matches(
        "def f():\n    x = y = 0\n    def g():\n        nonlocal x, y\n",
        "Nonlocal",
    );
}

#[test]
fn test_expressions() {
    parse_tree_matches("foo(1)", "Call");
    parse_tree_matches("[1, 2, 3]", "List");
    parse_tree_matches("[x**3 for x in range(20)]", "ListComprehension");
    parse_tree_matches("[x**3 for x in range(20) if x % 3]", "ListComprehension");
    parse_tree_matches(
        "[x**3 for x in range(20) if x % 2 if x % 3]",
        "ListComprehension",
    );
    parse_tree_matches("list(x**3 for x in range(20))", "Generator");
    parse_tree_matches("list(x**3 for x in range(20) if x % 3)", "Generator");
    parse_tree_matches(
        "list(x**3 for x in range(20) if x % 2 if x % 3)",
        "Generator",
    );
    parse_tree_matches("foo(*args)", "Call");
    parse_tree_matches("foo(*args, **kw)", "Call");
    parse_tree_matches("foo(**kw)", "Call");
    parse_tree_matches("foo(key=value)", "Call");
    parse_tree_matches("foo(key=value, *args)", "Call");
    parse_tree_matches("foo(key=value, *args, **kw)", "Call");
    parse_tree_matches("foo(key=value, **kw)", "Call");
    parse_tree_matches("foo(a, b, c, *args)", "Call");
    parse_tree_matches("foo(a, b, c, *args, **kw)", "Call");
    parse_tree_matches("foo(a, b, c, **kw)", "Call");
    parse_tree_matches("foo(a, *args, keyword=23)", "Call");
    parse_tree_matches("foo + bar", "Plus");
    parse_tree_matches("foo - bar", "Minus");
    parse_tree_matches("foo * bar", "Times");
    parse_tree_matches("foo / bar", "Divide");
    parse_tree_matches("foo // bar", "IntDivide");
    parse_tree_matches("(foo := 1)", "Walrus");
    parse_tree_matches("lambda: 0", "Lambda");
    parse_tree_matches("lambda: 0", "Number(Int(0)");
    parse_tree_matches("lambda x: 0", "Parameter { name: \"x\"");
    parse_tree_matches("lambda *y: 0", "starred: true");
    parse_tree_matches("lambda *y, **z: 0", "starred: true");
    parse_tree_matches("lambda *y, **z: 0", "double_starred: true");
    parse_tree_matches("lambda **z: 0", "double_starred: true");
    parse_tree_matches("lambda x, y: 0", "Parameter { name: \"y\"");
    parse_tree_matches("lambda foo=bar: 0", "default: Some(Name(\"bar\"");
    parse_tree_matches(
        "lambda foo=bar, spaz=nifty+spit: 0",
        "default: Some(BinaryOperation(",
    );
    parse_tree_matches("lambda foo=bar, **z: 0", "double_starred: true");
    parse_tree_matches("lambda foo=bar, blaz=blat+2, **z: 0", "Number(Int(0)");
    parse_tree_matches(
        "lambda foo=bar, blaz=blat+2, *, u, **z: 0",
        "keyword_only: true",
    );
    parse_tree_matches(
        "lambda foo=bar, blaz=blat+2, *y, u, **z: 0",
        "keyword_only: false",
    );
    parse_tree_matches("lambda x, /, *y, **z: 0", "positional_only: true");
    parse_tree_matches("(x for x in range(10))", "Generator");
    parse_tree_matches("...", "Ellipsis");
    parse_tree_matches("a[...]", "Slice");
}

#[test]
fn test_simple_expression() {
    parse_tree_matches("a", "Expressions([Name(\"a\"");
}

#[test]
fn test_simple_assignments() {
    parse_tree_matches("a = b", "Assignment");
    parse_tree_matches("a = b = c = d = e", "Assignment");
}

#[test]
fn test_var_annot() {
    parse_tree_matches("x: int = 5", "Int(5)");
    parse_tree_matches("y: List[T] = []; z: [list] = fun()", "Assignment");
    parse_tree_matches("x: tuple = (1, 2)", "Tuple");
    parse_tree_matches("d[f()]: int = 42", "Assignment([Slice");
    parse_tree_matches("x.y.z.w: complex = 42j", "Assignment([Subscript");
    parse_tree_matches("x: int", "int");
    parse_tree_matches("def f():\n    x: str\n    y: int = 5\n", "Assignment");
    parse_tree_matches("class C:\n    x: str\n    y: int = 5\n", "Assignment");
    parse_tree_matches(
        "class C:\n    def __init__(self, x: int) -> None:\n        self.x: int = x\n",
        "Assignment([Subscript",
    );
    assert_raises_error("2+2: int", "illegal target for annotation");
    assert_raises_error("f(d[x]): str = 'abc'", "illegal target for annotation");
    assert_raises_error("[]: int = 5", "only single targets can be annotated");
    assert_raises_error(
        "x, *y, z: int = range(5)",
        "only single target (not tuple) can be annotated",
    );
    assert_raises_error("x: int = 1, y = 2", "invalid syntax");
    assert_raises_error("u = v: int", "invalid syntax");
    assert_raises_error("False: int", "illegal target for annotation");
    assert_raises_error("x.False: int", "invalid syntax");
    assert_raises_error(
        "x.y,: int",
        "only single target (not tuple) can be annotated",
    );
    assert_raises_error("[0]: int", "only single targets can be annotated");
    assert_raises_error("f(): int", "illegal target for annotation");
}

#[test]
fn test_simple_augmented_assignments() {
    parse_tree_matches("a += b", "Plus");
    parse_tree_matches("a -= b", "Minus");
    parse_tree_matches("a *= b", "Times");
    parse_tree_matches("a /= b", "Divide");
    parse_tree_matches("a //= b", "IntDivide");
    parse_tree_matches("a %= b", "Modulo");
    parse_tree_matches("a &= b", "BitwiseAnd");
    parse_tree_matches("a |= b", "BitwiseOr");
    parse_tree_matches("a ^= b", "BitwiseXor");
    parse_tree_matches("a <<= b", "LeftShift");
    parse_tree_matches("a >>= b", "RightShift");
    parse_tree_matches("a **= b", "Power");
}

#[test]
fn test_function_defs() {
    parse_tree_matches("def f(): pass", "FunctionDeclaration");
    parse_tree_matches("def f(*args): pass", "starred: true");
    parse_tree_matches("def f(*args, **kw): pass", "double_starred: true");
    parse_tree_matches("def f(**kw): pass", "double_starred: true");
    parse_tree_matches(
        "def f(foo=bar): pass",
        "default: Some(Name(\"bar\", (1, 11)-(1, 14)))",
    );
    parse_tree_matches("def f(foo=bar, *args): pass", "starred: true");
    parse_tree_matches("def f(foo=bar, *args, **kw): pass", "double_starred: true");
    parse_tree_matches("def f(foo=bar, **kw): pass", "double_starred: true");

    parse_tree_matches(
        "def f(a, b): pass",
        "Parameter { name: \"a\", default: None",
    );
    parse_tree_matches(
        "def f(a, b, *args): pass",
        "Parameter { name: \"b\", default: None",
    );
    parse_tree_matches(
        "def f(a, b, *args, **kw): pass",
        "Parameter { name: \"args\", default: None",
    );
    parse_tree_matches(
        "def f(a, b, **kw): pass",
        "Parameter { name: \"kw\", default: None",
    );
    parse_tree_matches(
        "def f(a, b, foo=bar): pass",
        "Parameter { name: \"a\", default: None",
    );
    parse_tree_matches(
        "def f(a, b, foo=bar): pass",
        "default: Some(Name(\"bar\", (1, 17)-(1, 20)))",
    );
    parse_tree_matches("def f(a, b, foo=bar, *args): pass", "starred: true");
    parse_tree_matches("def f(a, b, foo=bar, *args, **kw): pass", "starred: true");
    parse_tree_matches("def f(a, b, foo=bar, **kw): pass", "double_starred: true");

    parse_tree_matches(
        "@staticmethod\ndef f(): pass",
        "Decorator(Name(\"staticmethod\"",
    );
    parse_tree_matches(
        "@staticmethod\n@funcattrs(x, y)\ndef f(): pass",
        "Decorator(Call(Name(\"funcattrs\"",
    );
    parse_tree_matches(
        "@funcattrs()\ndef f(): pass",
        "Decorator(Call(Name(\"funcattrs\"",
    );

    parse_tree_matches("def f(*, a): pass", "keyword_only: true");
    parse_tree_matches("def f(*, a = 5): pass", "default: Some(Number(Int(5)");
    parse_tree_matches("def f(*, a = 5, b): pass", "Parameter { name: \"b\"");
    parse_tree_matches("def f(*, a, b = 5): pass", "keyword_only: true");
    parse_tree_matches(
        "def f(*, a, b = 5, **kwds): pass",
        "Parameter { name: \"kwds\"",
    );
    parse_tree_matches("def f(*args, a): pass", "keyword_only: true");
    parse_tree_matches("def f(*args, a): pass", "keyword_only: false");
    parse_tree_matches("def f(*args, a = 5): pass", "default: Some(Number(Int(5)");
    parse_tree_matches("def f(*args, a = 5, b): pass", "Parameter { name: \"b\"");
    parse_tree_matches(
        "def f(*args, a, b = 5): pass",
        "default: Some(Number(Int(5)",
    );
    parse_tree_matches(
        "def f(*args, a, b = 5, **kwds): pass",
        "double_starred: true",
    );

    parse_tree_matches("def f(a, /): pass", "positional_only: true");
    parse_tree_matches("def f(a, /,): pass", "positional_only: true");
    parse_tree_matches("def f(a, b, /): pass", "positional_only: true");
    parse_tree_matches("def f(a, b, /, c): pass", "positional_only: true");
    parse_tree_matches("def f(a, b, /, c): pass", "positional_only: false");
    parse_tree_matches("def f(a, b, /, c = 6): pass", "default: Some(Number(Int(6)");
    parse_tree_matches("def f(a, b, /, c, *, d): pass", "keyword_only: true");
    parse_tree_matches(
        "def f(a, b, /, c = 1, *, d): pass",
        "default: Some(Number(Int(1)",
    );
    parse_tree_matches(
        "def f(a, b, /, c, *, d = 1): pass",
        "default: Some(Number(Int(1)",
    );
    parse_tree_matches(
        "def f(a, b=1, /, c=2, *, d = 3): pass",
        "default: Some(Number(Int(1)",
    );
    parse_tree_matches(
        "def f(a=0, b=1, /, c=2, *, d = 3): pass",
        "default: Some(Number(Int(1)",
    );

    parse_tree_matches("def f(a: int): pass", "Some(Name(\"int\"");
    parse_tree_matches("def f(a: int = 5): pass", "default: Some(Number(Int(5)");
    parse_tree_matches("def f(*args: list): pass", "Some(Name(\"list\"");
    parse_tree_matches("def f(**kwds: dict): pass", "Some(Name(\"dict\"");
    parse_tree_matches("def f(*, a: int): pass", "Some(Name(\"int\"");
    parse_tree_matches("def f(*, a: int = 5): pass", "default: Some(Number(Int(5)");
    parse_tree_matches("def f() -> int: pass", "return_type: Some(Name(\"int\"");
}

#[test]
fn test_class_defs() {
    parse_tree_matches("class foo():pass", "ClassDefinition");
    parse_tree_matches("class foo:pass", "ClassDefinition");
    parse_tree_matches("class foo():pass", "body: [Pass");
    parse_tree_matches(
        "class foo(object):pass",
        "ancestors: Arguments { positional: [Name(\"object\"",
    );
    parse_tree_matches(
        "@class_decorator\nclass foo():pass",
        "Decorator(Name(\"class_decorator\"",
    );
    parse_tree_matches(
        "@class_decorator(arg)\nclass foo():pass",
        "Decorator(Call(Name(\"class_decorator\"",
    );
    parse_tree_matches(
        "@decorator1\n@decorator2\nclass foo():pass",
        "Decorator(Name(\"decorator1\"",
    );
    parse_tree_matches(
        "@decorator1\n@decorator2\nclass foo():pass",
        "Decorator(Name(\"decorator2\"",
    );
}

#[test]
fn test_import_from_statement() {
    parse_tree_matches("from sys.path import *", "path: [\"sys\", \"path\"]");
    parse_tree_matches("from sys.path import *", "items: []");
    parse_tree_matches(
        "from sys.path import dirname",
        "items: [ImportItem { name: \"dirname\"",
    );
    parse_tree_matches(
        "from sys.path import (dirname)",
        "items: [ImportItem { name: \"dirname\"",
    );
    parse_tree_matches(
        "from sys.path import (dirname,)",
        "items: [ImportItem { name: \"dirname\"",
    );
    parse_tree_matches(
        "from sys.path import dirname as my_dirname",
        "items: [ImportItem { name: \"dirname\"",
    );
    parse_tree_matches(
        "from sys.path import dirname as my_dirname",
        "alias: Some(\"my_dirname\")",
    );
    parse_tree_matches(
        "from sys.path import (dirname as my_dirname)",
        "items: [ImportItem { name: \"dirname\"",
    );
    parse_tree_matches(
        "from sys.path import (dirname as my_dirname,)",
        "alias: Some(\"my_dirname\")",
    );
    parse_tree_matches(
        "from sys.path import dirname, basename",
        "ImportItem { name: \"basename\"",
    );
    parse_tree_matches(
        "from sys.path import (dirname, basename)",
        "ImportItem { name: \"dirname\"",
    );
    parse_tree_matches(
        "from sys.path import (dirname, basename,)",
        "ImportItem { name: \"basename\"",
    );
    parse_tree_matches(
        "from sys.path import dirname as my_dirname, basename",
        "ImportItem { name: \"dirname\"",
    );
    parse_tree_matches(
        "from sys.path import dirname as my_dirname, basename",
        "alias: Some(\"my_dirname\")",
    );
    parse_tree_matches(
        "from sys.path import dirname as my_dirname, basename",
        "ImportItem { name: \"basename\"",
    );
    parse_tree_matches(
        "from sys.path import (dirname as my_dirname, basename)",
        "ImportItem { name: \"basename\"",
    );
    parse_tree_matches(
        "from sys.path import (dirname as my_dirname, basename,)",
        "ImportItem { name: \"basename\"",
    );
    parse_tree_matches(
        "from sys.path import dirname, basename as my_basename",
        "ImportItem { name: \"basename\"",
    );
    parse_tree_matches(
        "from sys.path import (dirname, basename as my_basename)",
        "alias: Some(\"my_basename\")",
    );
    parse_tree_matches(
        "from sys.path import (dirname, basename as my_basename,)",
        "alias: Some(\"my_basename\")",
    );
    parse_tree_matches("from .bogus import x", "rel_level: 1");
    parse_tree_matches("from .bogus import x", "ImportItem { name: \"x\"");
}

#[test]
fn test_basic_import_statement() {
    parse_tree_matches(
        "import sys",
        "Import { module: Module { rel_level: 0, path: [\"sys\"], alias: None }, items: [] }",
    );
    parse_tree_matches("import sys as system", "alias: Some(\"system\")");
    parse_tree_matches("import sys, math", "path: [\"sys\"]");
    parse_tree_matches("import sys, math", "path: [\"math\"]");
    parse_tree_matches("import sys as system, math", "path: [\"sys\"]");
    parse_tree_matches("import sys as system, math", "alias: Some(\"system\")");
    parse_tree_matches("import sys as system, math", "path: [\"math\"]");
    parse_tree_matches("import sys, math as my_math", "alias: Some(\"my_math\"");
}

#[test]
fn test_relative_imports() {
    parse_tree_matches("from . import name", 
        "Import { module: Module { rel_level: 1, path: [], alias: None }, items: [ImportItem { name: \"name\", alias: None }] }"
    );
    parse_tree_matches("from .. import name", "rel_level: 2");
    parse_tree_matches("from ... import name", "rel_level: 3");
    parse_tree_matches("from .... import name", "rel_level: 4");
    parse_tree_matches("from .pkg import name", 
        "Import { module: Module { rel_level: 1, path: [\"pkg\"], alias: None }, items: [ImportItem { name: \"name\", alias: None }] }"
    );
    parse_tree_matches("from ..pkg import name", "rel_level: 2");
    parse_tree_matches("from ...pkg import name", "rel_level: 3");
    parse_tree_matches("from ....pkg import name", "rel_level: 4");
}

#[test]
fn test_pep263() {
    parse_tree_matches("# -*- coding: iso-8859-1 -*-\npass\n", "Pass");
}

#[test]
fn test_assert() {
    parse_tree_matches(
        "assert alo < ahi and blo < bhi\n",
        "Assert(BinaryOperation(And",
    );
}

#[test]
fn test_with() {
    parse_tree_matches(
        "with open('x'): pass\n",
        "With([WithItem(Call(Name(\"open\"",
    );
    parse_tree_matches("with open('x'): pass\n", "Strings([Literal(\"x\"");
    parse_tree_matches("with open('x'): pass\n", "[Pass((1, 17)-(1, 21))]");
    parse_tree_matches("with open('x') as f: pass\n", "Some(Name(\"f\"");
    parse_tree_matches(
        "with open('x') as f, open('y') as g: pass\n",
        "Literal(\"x\"",
    );
    parse_tree_matches(
        "with open('x') as f, open('y') as g: pass\n",
        "Literal(\"y\"",
    );
    parse_tree_matches(
        "with open('x') as f, open('y') as g: pass\n",
        "Some(Name(\"f\"",
    );
}

#[test]
fn test_try_stmt() {
    parse_tree_matches("try: pass\nexcept: pass\n", "[Try([Pass");
    parse_tree_matches("try: pass\nexcept: pass\n", "ExceptBlock");
    parse_tree_matches(
        "try: pass\nfinally: revenge\n",
        "Some([Expressions([Name(\"revenge\"",
    );
    parse_tree_matches(
        "try: pass\nexcept A: return\nfinally: revenge\n",
        "ExceptBlock(Some(Name(\"A\"",
    );
    parse_tree_matches(
        "try: pass\nexcept A: return\nfinally: revenge\n",
        "Some([Expressions([Name(\"revenge\"",
    );
    parse_tree_matches(
        "try: pass\nexcept A: return\nfinally: revenge\n",
        "Return([]",
    );
    parse_tree_matches(
        "try: pass\nexcept A: pass\nexcept: pass\nfinally: pass\n",
        "ExceptBlock(Some(Name(\"A\"",
    );
    parse_tree_matches(
        "try: pass\nexcept A: pass\nexcept: pass\nfinally: pass\n",
        "ExceptBlock(None",
    );
    parse_tree_matches(
        "try: pass\nexcept: pass\nelse: revenge\n",
        "Some([Expressions([Name(\"revenge\"",
    );
    parse_tree_matches(
        "try: pass\nexcept: pass\nelse: teapot\nfinally: revenge\n",
        "Some([Expressions([Name(\"revenge\"",
    );
    parse_tree_matches(
        "try: pass\nexcept: pass\nelse: teapot\nfinally: revenge\n",
        "Some([Expressions([Name(\"teapot\"",
    );
}

#[test]
fn test_if_stmt() {
    parse_tree_matches(
        "if condition:\n  steak\nelse:\n  soup\n",
        "Name(\"condition\"",
    );
    parse_tree_matches(
        "if condition:\n  steak\nelse:\n  soup\n",
        "[Expressions([Name(\"steak\"",
    );
    parse_tree_matches(
        "if condition:\n  steak\nelse:\n  soup\n",
        "Some([Expressions([Name(\"soup\"",
    );
    parse_tree_matches(
        "if True:\n  pass\nelif something_else:\n  drop_teapot\nelse:\n  bazinga\n",
        "[(Name(\"something_else\", (3, 6)-(3, 20)), [Expressions([Name(\"drop_teapot\"",
    );
    parse_tree_matches(
        "if True:\n  pass\nelif something_else:\n  drop_teapot\nelse:\n  bazinga\n",
        "Some([Expressions([Name(\"bazinga\"",
    );
}

#[test]
fn test_extended_unpacking() {
    parse_tree_matches("*a = y", "Assignment([ListUnwrap(Name(\"a\"");
    parse_tree_matches("*a = y", "Some([Name(\"y\"");
    parse_tree_matches("x, *b, = m", "ListUnwrap(Name(\"b\"");
    parse_tree_matches("[*a, *b] = y", "List([ListUnwrap(Name(\"a\"");
    parse_tree_matches("[*a, *b] = y", "ListUnwrap(Name(\"b\"");
    parse_tree_matches("for [*x, b] in x: pass", "List([ListUnwrap(Name(\"x\"");
}

#[test]
fn test_raise_statement() {
    parse_tree_matches("raise\n", "Raise(None, None, (1, 1)-(1, 6))");
    parse_tree_matches("raise e\n", "Raise(Some(Name(\"e\"");
    parse_tree_matches(
        "try:\n    suite\nexcept Exception as e:\n    raise ValueError from e\n",
        "Raise(Some(Name(\"ValueError\"",
    );
    parse_tree_matches(
        "try:\n    suite\nexcept Exception as e:\n    raise ValueError from e\n",
        "Some(Name(\"e\", (4, 27)-(4, 28)))",
    );
}

#[test]
fn test_list_displays() {
    parse_tree_matches("[]", "Expressions([List([]");
    parse_tree_matches("[*{2}, 3, *[4]]", "List([ListUnwrap(Set([Number(Int(2)");
    parse_tree_matches("[*{2}, 3, *[4]]", "ListUnwrap(List([Number(Int(4)");
}

#[test]
fn test_set_displays() {
    parse_tree_matches("{*{2}, 3, *[4]}", "Set([ListUnwrap(Set([Number(Int(2)");
    parse_tree_matches("{*{2}, 3, *[4]}", "ListUnwrap(List([Number(Int(4)");
    parse_tree_matches("{2}", "Expressions([Set([Number(Int(2)");
    parse_tree_matches("{2,}", "Set([Number(Int(2)");
    parse_tree_matches("{2, 3}", "Set([Number(Int(2)");
    parse_tree_matches("{2, 3}", "Int(3)");
    parse_tree_matches("{2, 3,}", "Set([Number(Int(2)");
    parse_tree_matches("{2, 3,}", "Int(3)");
}

#[test]
fn test_dict_displays() {
    parse_tree_matches("{}", "Expressions([Dict([]");
    parse_tree_matches("{a:b}", "Expressions([Dict([Tuple([");
    parse_tree_matches("{a:b,}", "Expressions([Dict([Tuple([");
    parse_tree_matches("{a:b, c:d}", "Expressions([Dict([Tuple([");
    parse_tree_matches("{a:b, c:d}", "Name(\"a\"");
    parse_tree_matches("{a:b, c:d}", ", Name(\"b\"");
    parse_tree_matches("{a:b, c:d}", "Name(\"c\"");
    parse_tree_matches("{a:b, c:d}", ", Name(\"d\"");
    parse_tree_matches("{a:b, c:d,}", "Expressions([Dict([Tuple([");
    parse_tree_matches("{**{}}", "Expressions([Dict([DictUnwrap(Dict([]");
    parse_tree_matches(
        "{**{}, 3:4, **{5:6, 7:8}}",
        "DictUnwrap(Dict([Tuple([Number(Int(5",
    );
}

#[test]
fn test_argument_unpacking() {
    parse_tree_matches("f(*a, **b)", "ListUnwrap(Name(\"a\"");
    parse_tree_matches("f(*a, **b)", "DictUnwrap(Name(\"b\"");
    parse_tree_matches("f(a, *b, *c, *d)", "ListUnwrap(Name(\"b\"");
    parse_tree_matches("f(**a, **b)", "DictUnwrap(Name(\"a\"");
    parse_tree_matches("f(2, *a, *b, **b, **c, **d)", "ListUnwrap(Name(\"b\"");
    parse_tree_matches("f(2, *a, *b, **b, **c, **d)", "DictUnwrap(Name(\"b\"");
    parse_tree_matches(
        "f(*b, *() or () and (), **{} and {}, **() or {})",
        "DictUnwrap(BinaryOperation(",
    );
}

#[test]
fn test_set_comprehensions() {
    parse_tree_matches("{x for x in seq}", "SetComprehension(Name(\"x\"");
    parse_tree_matches("{x for x in seq}", "ForIfClause([Name(\"x\"");
    parse_tree_matches("{f(x) for x in seq}", "SetComprehension(Call(Name(\"f\"");
    parse_tree_matches(
        "{f(x) for x in seq if condition(x)}",
        "[Call(Name(\"condition\"",
    );
}

#[test]
fn test_dict_comprehensions() {
    parse_tree_matches("{x:x for x in seq}", "DictComprehension(Tuple([Name(\"x\"");
    parse_tree_matches("{x:x for x in seq}", "ForIfClause([Name(\"x\"");
    parse_tree_matches(
        "{x**2:x[3] for x in seq if condition(x)}",
        "[Call(Name(\"condition\"",
    );
    parse_tree_matches(
        "{x:x for x in seq1 for y in seq2 if condition(x, y)}",
        "[ForIfClause([Name(\"x\"",
    );
    parse_tree_matches(
        "{x:x for x in seq1 for y in seq2 if condition(x, y)}",
        "ForIfClause([Name(\"y\"",
    );
}

#[test]
fn test_named_expressions() {
    parse_tree_matches("(a := a)", "Walrus(Name(\"a\"");
    parse_tree_matches("(a := 1)", "Number(Int(1)");
    parse_tree_matches(
        "if (match := pattern.search(data)) is None: pass",
        "Walrus(Name(\"match\"",
    );
    parse_tree_matches(
        "while match := pattern.search(f.read()): pass",
        "Walrus(Name(\"match\"",
    );
    parse_tree_matches("[y := f(x), y**2, y**3]", "Walrus(Name(\"y\"");
    parse_tree_matches(
        "filtered_data = [y for x in data if (y := f(x)) is None]",
        "Walrus(Name(\"y\"",
    );
    parse_tree_matches("(y := f(x))", "Call(Name(\"f\"");
    parse_tree_matches("y0 = (y1 := f(x))", "Walrus(Name(\"y1\"");
    parse_tree_matches("foo(x=(y := f(x)))", "Walrus(Name(\"y\"");
    parse_tree_matches("def foo(answer=(p := 42)): pass", "Walrus(Name(\"p\"");
    parse_tree_matches("def foo(answer: (p := 42) = 5): pass", "Walrus(Name(\"p\"");
    parse_tree_matches("lambda: (x := 1)", "Walrus(Name(\"x\"");
    parse_tree_matches("(x := lambda: 1)", "Walrus(Name(\"x\"");
    parse_tree_matches("(x := lambda: (y := 1))", "Walrus(Name(\"y\"");
    parse_tree_matches(
        "lambda line: (m := re.match(pattern, line)) and m.group(1)",
        "Walrus(Name(\"m\"",
    );
    parse_tree_matches("x = (y := 0)", "Walrus(Name(\"y\"");
    parse_tree_matches("(z:=(y:=(x:=0)))", "Walrus(Name(\"x\"");
    parse_tree_matches("(z:=(y:=(x:=0)))", "Walrus(Name(\"y\"");
    parse_tree_matches("(z:=(y:=(x:=0)))", "Walrus(Name(\"z\"");
    parse_tree_matches("(info := (name, phone, *rest))", "Walrus(Name(\"info\"");
    parse_tree_matches("(x:=1,2)", "Tuple([Walrus(Name(\"x\"");
    parse_tree_matches(
        "(total := total + tax)",
        "BinaryOperation(Plus, Name(\"total\"",
    );
    parse_tree_matches("len(lines := f.readlines())", "Call(Subscript(Name(\"f\"");
    parse_tree_matches("foo(x := 3, cat='vector')", "Walrus(Name(\"x\"");
    parse_tree_matches(
        "foo(cat=(category := 'vector'))",
        "Walrus(Name(\"category\"",
    );
    parse_tree_matches(
        "if any(len(longline := l) >= 100 for l in lines): print(longline)",
        "Walrus(Name(\"longline\"",
    );
    parse_tree_matches(
        "if env_base := os.environ.get('PYTHONUSERBASE', None): return env_base",
        "Walrus(Name(\"env_base\"",
    );
    parse_tree_matches(
        "if self._is_special and (ans := self._check_nans(context=context)): return ans",
        "Walrus(Name(\"ans\"",
    );
    parse_tree_matches("foo(b := 2, a=1)", "Walrus(Name(\"b\"");
    parse_tree_matches("foo(b := 2, a=1)", "Walrus(Name(\"b\"");
    parse_tree_matches("foo((b := 2), a=1)", "Walrus(Name(\"b\"");
    parse_tree_matches("foo(c=(b := 2), a=1)", "Walrus(Name(\"b\"");
    parse_tree_matches("{(x := C(i)).q: x for i in y}", "Walrus(Name(\"x\"");
}
