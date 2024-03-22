# prometheus
A hand rolled Python parser, written using parser combinators

This is an exercise in using parser combinators to write a parser for a nontrivial grammar (in this case Python), with a focus on good error messages.
It is fairly feature-complete, notable exceptions being:

- String parsing is a bit rudimentary (and partially incomplete), thus needs some work
- Generated error messages are generally at the level of the CPython parser, but error recovery needs some work, and pretty-printing is not available
- type comments are not parsed

Its performance is generally decent, with a fairly complex 3000 line script taking ~88ms to parse (compared to ~33ms with the CPython parser, compare `benchmark.py`), which, for a one week one person toy project isn't too bad, I suppose.
