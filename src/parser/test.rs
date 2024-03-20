use super::{error::Error, parse, tokenize_string, Block};

fn parse_string(input: &str) -> Result<(Block, Vec<Error>), String> {
    let tokens = tokenize_string(input)?;
    let (tree, errors) = parse(&tokens);
    Ok((tree, errors))
}

fn assert_successful_parse(input: &str) {
    assert!(matches!(parse_string(input), Ok((_, errors)) if errors.is_empty()))
}

fn assert_raises_error(input: &str) {
    assert!(matches!(parse_string(input), Ok((_, errors)) if !errors.is_empty()))
}

#[test]
fn test_yield_statement() {
    assert_successful_parse("def f(): yield 1");
    assert_successful_parse("def f(): yield");
    assert_successful_parse("def f(): x += yield");
    assert_successful_parse("def f(): x = yield 1");
    assert_successful_parse("def f(): x = y = yield 1");
    assert_successful_parse("def f(): x = yield");
    assert_successful_parse("def f(): x = y = yield");
    assert_successful_parse("def f(): 1 + (yield)*2");
    assert_successful_parse("def f(): (yield 1)*2");
    assert_successful_parse("def f(): return; yield 1");
    assert_successful_parse("def f(): yield 1; return");
    assert_successful_parse("def f(): yield from 1");
    assert_successful_parse("def f(): x = yield from 1");
    assert_successful_parse("def f(): f((yield from 1))");
    assert_successful_parse("def f(): yield 1; return 1");
    assert_successful_parse("def f():\n    for x in range(30):\n        yield x\n");
    assert_successful_parse("def f():\n    if (yield):\n        yield x\n");
}

#[test]
fn test_await_statement() {
    assert_successful_parse("async def f():\n await smth()");
    assert_successful_parse("async def f():\n foo = await smth()");
    assert_successful_parse("async def f():\n foo, bar = await smth()");
    assert_successful_parse("async def f():\n (await smth())");
    assert_successful_parse("async def f():\n foo((await smth()))");
    assert_successful_parse("async def f():\n await foo(); return 42");
}

#[test]
fn test_async_with_statement() {
    assert_successful_parse("async def f():\n async with 1: pass");
    assert_successful_parse("async def f():\n async with a as b, c as d: pass");
}

#[test]
fn test_async_for_statement() {
    assert_successful_parse("async def f():\n async for i in (): pass");
    assert_successful_parse("async def f():\n async for i, b in (): pass");
}

#[test]
fn test_nonlocal_statement() {
    assert_successful_parse("def f():\n    x = 0\n    def g():\n        nonlocal x\n");
    assert_successful_parse("def f():\n    x = y = 0\n    def g():\n        nonlocal x, y\n");
}

#[test]
fn test_expressions() {
    assert_successful_parse("foo(1)");
    assert_successful_parse("[1, 2, 3]");
    assert_successful_parse("[x**3 for x in range(20)]");
    assert_successful_parse("[x**3 for x in range(20) if x % 3]");
    assert_successful_parse("[x**3 for x in range(20) if x % 2 if x % 3]");
    assert_successful_parse("list(x**3 for x in range(20))");
    assert_successful_parse("list(x**3 for x in range(20) if x % 3)");
    assert_successful_parse("list(x**3 for x in range(20) if x % 2 if x % 3)");
    assert_successful_parse("foo(*args)");
    assert_successful_parse("foo(*args, **kw)");
    assert_successful_parse("foo(**kw)");
    assert_successful_parse("foo(key=value)");
    assert_successful_parse("foo(key=value, *args)");
    assert_successful_parse("foo(key=value, *args, **kw)");
    assert_successful_parse("foo(key=value, **kw)");
    assert_successful_parse("foo(a, b, c, *args)");
    assert_successful_parse("foo(a, b, c, *args, **kw)");
    assert_successful_parse("foo(a, b, c, **kw)");
    assert_successful_parse("foo(a, *args, keyword=23)");
    assert_successful_parse("foo + bar");
    assert_successful_parse("foo - bar");
    assert_successful_parse("foo * bar");
    assert_successful_parse("foo / bar");
    assert_successful_parse("foo // bar");
    assert_successful_parse("(foo := 1)");
    assert_successful_parse("lambda: 0");
    assert_successful_parse("lambda x: 0");
    assert_successful_parse("lambda *y: 0");
    assert_successful_parse("lambda *y, **z: 0");
    assert_successful_parse("lambda **z: 0");
    assert_successful_parse("lambda x, y: 0");
    assert_successful_parse("lambda foo=bar: 0");
    assert_successful_parse("lambda foo=bar, spaz=nifty+spit: 0");
    assert_successful_parse("lambda foo=bar, **z: 0");
    assert_successful_parse("lambda foo=bar, blaz=blat+2, **z: 0");
    assert_successful_parse("lambda foo=bar, blaz=blat+2, *y, **z: 0");
    assert_successful_parse("lambda x, *y, **z: 0");
    assert_successful_parse("(x for x in range(10))");
    assert_successful_parse("foo(x for x in range(10))");
    assert_successful_parse("...");
    assert_successful_parse("a[...]");
}

#[test]
fn test_simple_expression() {
    assert_successful_parse("a");
}

#[test]
fn test_simple_assignments() {
    assert_successful_parse("a = b");
    assert_successful_parse("a = b = c = d = e");
}

#[test]
fn test_var_annot() {
    assert_successful_parse("x: int = 5");
    assert_successful_parse("y: List[T] = []; z: [list] = fun()");
    assert_successful_parse("x: tuple = (1, 2)");
    assert_successful_parse("d[f()]: int = 42");
    // assert_successful_parse("f(d[x]): str = 'abc'");
    assert_successful_parse("x.y.z.w: complex = 42j");
    assert_successful_parse("x: int");
    assert_successful_parse("def f():\n    x: str\n    y: int = 5\n");
    assert_successful_parse("class C:\n    x: str\n    y: int = 5\n");
    assert_successful_parse(
        "class C:\n    def __init__(self, x: int) -> None:\n        self.x: int = x\n",
    );
    // assert_raises_error("2+2: int");
    // assert_raises_error("[]: int = 5");
    // assert_raises_error("x, *y, z: int = range(5)");
    // assert_raises_error("x: int = 1, y = 2");
    // assert_raises_error("u = v: int");
    // assert_raises_error("False: int");
    // assert_raises_error("x.False: int");
    // assert_raises_error("x.y,: int");
    // assert_raises_error("[0]: int");
    // assert_raises_error("f(): int");
}

#[test]
fn test_simple_augmented_assignments() {
    assert_successful_parse("a += b");
    assert_successful_parse("a -= b");
    assert_successful_parse("a *= b");
    assert_successful_parse("a /= b");
    assert_successful_parse("a //= b");
    assert_successful_parse("a %= b");
    assert_successful_parse("a &= b");
    assert_successful_parse("a |= b");
    assert_successful_parse("a ^= b");
    assert_successful_parse("a <<= b");
    assert_successful_parse("a >>= b");
    assert_successful_parse("a **= b");
}

#[test]
fn test_function_defs() {
    assert_successful_parse("def f(): pass");
    assert_successful_parse("def f(*args): pass");
    assert_successful_parse("def f(*args, **kw): pass");
    assert_successful_parse("def f(**kw): pass");
    assert_successful_parse("def f(foo=bar): pass");
    assert_successful_parse("def f(foo=bar, *args): pass");
    assert_successful_parse("def f(foo=bar, *args, **kw): pass");
    assert_successful_parse("def f(foo=bar, **kw): pass");

    assert_successful_parse("def f(a, b): pass");
    assert_successful_parse("def f(a, b, *args): pass");
    assert_successful_parse("def f(a, b, *args, **kw): pass");
    assert_successful_parse("def f(a, b, **kw): pass");
    assert_successful_parse("def f(a, b, foo=bar): pass");
    assert_successful_parse("def f(a, b, foo=bar, *args): pass");
    assert_successful_parse("def f(a, b, foo=bar, *args, **kw): pass");
    assert_successful_parse("def f(a, b, foo=bar, **kw): pass");

    assert_successful_parse("@staticmethod\ndef f(): pass");
    assert_successful_parse("@staticmethod\n@funcattrs(x, y)\ndef f(): pass");
    assert_successful_parse("@funcattrs()\ndef f(): pass");

    assert_successful_parse("def f(*, a): pass");
    assert_successful_parse("def f(*, a = 5): pass");
    assert_successful_parse("def f(*, a = 5, b): pass");
    assert_successful_parse("def f(*, a, b = 5): pass");
    assert_successful_parse("def f(*, a, b = 5, **kwds): pass");
    assert_successful_parse("def f(*args, a): pass");
    assert_successful_parse("def f(*args, a = 5): pass");
    assert_successful_parse("def f(*args, a = 5, b): pass");
    assert_successful_parse("def f(*args, a, b = 5): pass");
    assert_successful_parse("def f(*args, a, b = 5, **kwds): pass");

    assert_successful_parse("def f(a, /): pass");
    assert_successful_parse("def f(a, /,): pass");
    assert_successful_parse("def f(a, b, /): pass");
    assert_successful_parse("def f(a, b, /, c): pass");
    assert_successful_parse("def f(a, b, /, c = 6): pass");
    assert_successful_parse("def f(a, b, /, c, *, d): pass");
    assert_successful_parse("def f(a, b, /, c = 1, *, d): pass");
    assert_successful_parse("def f(a, b, /, c, *, d = 1): pass");
    assert_successful_parse("def f(a, b=1, /, c=2, *, d = 3): pass");
    assert_successful_parse("def f(a=0, b=1, /, c=2, *, d = 3): pass");

    assert_successful_parse("def f(a: int): pass");
    assert_successful_parse("def f(a: int = 5): pass");
    assert_successful_parse("def f(*args: list): pass");
    assert_successful_parse("def f(**kwds: dict): pass");
    assert_successful_parse("def f(*, a: int): pass");
    assert_successful_parse("def f(*, a: int = 5): pass");
    assert_successful_parse("def f() -> int: pass");
}

#[test]
fn test_class_defs() {
    assert_successful_parse("class foo():pass");
    assert_successful_parse("class foo(object):pass");
    assert_successful_parse("@class_decorator\nclass foo():pass");
    assert_successful_parse("@class_decorator(arg)\nclass foo():pass");
    assert_successful_parse("@decorator1\n@decorator2\nclass foo():pass");
}

#[test]
fn test_import_from_statement() {
    assert_successful_parse("from sys.path import *");
    assert_successful_parse("from sys.path import dirname");
    assert_successful_parse("from sys.path import (dirname)");
    assert_successful_parse("from sys.path import (dirname,)");
    assert_successful_parse("from sys.path import dirname as my_dirname");
    assert_successful_parse("from sys.path import (dirname as my_dirname)");
    assert_successful_parse("from sys.path import (dirname as my_dirname,)");
    assert_successful_parse("from sys.path import dirname, basename");
    assert_successful_parse("from sys.path import (dirname, basename)");
    assert_successful_parse("from sys.path import (dirname, basename,)");
    assert_successful_parse(
        "from sys.path import dirname as my_dirname, basename");
    assert_successful_parse(
        "from sys.path import (dirname as my_dirname, basename)");
    assert_successful_parse(
        "from sys.path import (dirname as my_dirname, basename,)");
    assert_successful_parse(
        "from sys.path import dirname, basename as my_basename");
    assert_successful_parse(
        "from sys.path import (dirname, basename as my_basename)");
    assert_successful_parse(
        "from sys.path import (dirname, basename as my_basename,)");
    assert_successful_parse("from .bogus import x");
}

#[test]
fn test_basic_import_statement() {
    assert_successful_parse("import sys");
    assert_successful_parse("import sys as system");
    assert_successful_parse("import sys, math");
    assert_successful_parse("import sys as system, math");
    assert_successful_parse("import sys, math as my_math");
}

#[test]
fn test_relative_imports() {
    assert_successful_parse("from . import name");
    assert_successful_parse("from .. import name");
    assert_successful_parse("from ... import name");
    assert_successful_parse("from .... import name");
    assert_successful_parse("from .pkg import name");
    assert_successful_parse("from ..pkg import name");
    assert_successful_parse("from ...pkg import name");
    assert_successful_parse("from ....pkg import name");
}

#[test]
fn test_pep263() {
    assert_successful_parse("# -*- coding: iso-8859-1 -*-\npass\n");
}

#[test]
fn test_assert() {
    assert_successful_parse("assert alo < ahi and blo < bhi\n");
}

#[test]
fn test_with() {
    assert_successful_parse("with open('x'): pass\n");
    assert_successful_parse("with open('x') as f: pass\n");
    assert_successful_parse("with open('x') as f, open('y') as g: pass\n");
}

#[test]
fn test_try_stmt() {
    assert_successful_parse("try: pass\nexcept: pass\n");
    assert_successful_parse("try: pass\nfinally: pass\n");
    assert_successful_parse("try: pass\nexcept A: pass\nfinally: pass\n");
    assert_successful_parse("try: pass\nexcept A: pass\nexcept: pass\nfinally: pass\n");
    assert_successful_parse("try: pass\nexcept: pass\nelse: pass\n");
    assert_successful_parse("try: pass\nexcept: pass\nelse: pass\nfinally: pass\n");
}

#[test]
fn test_if_stmt() {
    assert_successful_parse("if True:\n  pass\nelse:\n  pass\n");
    assert_successful_parse("if True:\n  pass\nelif True:\n  pass\nelse:\n  pass\n");
}

#[test]
fn test_extended_unpacking() {
    assert_successful_parse("*a = y");
    assert_successful_parse("x, *b, = m");
    assert_successful_parse("[*a, *b] = y");
    assert_successful_parse("for [*x, b] in x: pass");
}

#[test]
fn test_raise_statement() {
    assert_successful_parse("raise\n");
    assert_successful_parse("raise e\n");
    assert_successful_parse("try:\n    suite\nexcept Exception as e:\n    raise ValueError from e\n");
}

#[test]
fn test_list_displays() {
    assert_successful_parse("[]");
    assert_successful_parse("[*{2}, 3, *[4]]");
}

#[test]
fn test_set_displays() {
    assert_successful_parse("{*{2}, 3, *[4]}");
    assert_successful_parse("{2}");
    assert_successful_parse("{2,}");
    assert_successful_parse("{2, 3}");
    assert_successful_parse("{2, 3,}");
}

#[test]
fn test_dict_displays() {
    assert_successful_parse("{}");
    assert_successful_parse("{a:b}");
    assert_successful_parse("{a:b,}");
    assert_successful_parse("{a:b, c:d}");
    assert_successful_parse("{a:b, c:d,}");
    assert_successful_parse("{**{}}");
    assert_successful_parse("{**{}, 3:4, **{5:6, 7:8}}");
}

#[test]
fn test_argument_unpacking() {
    assert_successful_parse("f(*a, **b)");
    assert_successful_parse("f(a, *b, *c, *d)");
    assert_successful_parse("f(**a, **b)");
    assert_successful_parse("f(2, *a, *b, **b, **c, **d)");
    assert_successful_parse("f(*b, *() or () and (), **{} and {}, **() or {})");
}

#[test]
fn test_set_comprehensions() {
    assert_successful_parse("{x for x in seq}");
    assert_successful_parse("{f(x) for x in seq}");
    assert_successful_parse("{f(x) for x in seq if condition(x)}");
}

#[test]
fn test_dict_comprehensions() {
    assert_successful_parse("{x:x for x in seq}");
    assert_successful_parse("{x**2:x[3] for x in seq if condition(x)}");
    assert_successful_parse("{x:x for x in seq1 for y in seq2 if condition(x, y)}");
}

#[test]
fn test_named_expressions() {
    assert_successful_parse("(a := 1)");
    assert_successful_parse("(a := a)");
    assert_successful_parse("if (match := pattern.search(data)) is None: pass");
    assert_successful_parse("while match := pattern.search(f.read()): pass");
    assert_successful_parse("[y := f(x), y**2, y**3]");
    assert_successful_parse("filtered_data = [y for x in data if (y := f(x)) is None]");
    assert_successful_parse("(y := f(x))");
    assert_successful_parse("y0 = (y1 := f(x))");
    assert_successful_parse("foo(x=(y := f(x)))");
    assert_successful_parse("def foo(answer=(p := 42)): pass");
    assert_successful_parse("def foo(answer: (p := 42) = 5): pass");
    assert_successful_parse("lambda: (x := 1)");
    assert_successful_parse("(x := lambda: 1)");
    assert_successful_parse("(x := lambda: (y := 1))");
    assert_successful_parse("lambda line: (m := re.match(pattern, line)) and m.group(1)");
    assert_successful_parse("x = (y := 0)");
    assert_successful_parse("(z:=(y:=(x:=0)))");
    assert_successful_parse("(info := (name, phone, *rest))");
    assert_successful_parse("(x:=1,2)");
    assert_successful_parse("(total := total + tax)");
    assert_successful_parse("len(lines := f.readlines())");
    assert_successful_parse("foo(x := 3, cat='vector')");
    assert_successful_parse("foo(cat=(category := 'vector'))");
    assert_successful_parse("if any(len(longline := l) >= 100 for l in lines): print(longline)");
    assert_successful_parse(
        "if env_base := os.environ.get('PYTHONUSERBASE', None): return env_base"
    );
    assert_successful_parse(
        "if self._is_special and (ans := self._check_nans(context=context)): return ans"
    );
    assert_successful_parse("foo(b := 2, a=1)");
    assert_successful_parse("foo(b := 2, a=1)");
    assert_successful_parse("foo((b := 2), a=1)");
    assert_successful_parse("foo(c=(b := 2), a=1)");
    assert_successful_parse("{(x := C(i)).q: x for i in y}");
}
