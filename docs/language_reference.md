# Language reference

L is untyped and evaluates arguments before applying functions (call by value).
Functions capture the environment where they are created, not where they are called.

## Statements and names

Each statement occupies one line. A statement is a definition (`name = expression`)
or an expression. Blank lines and `--` line comments are ignored. A statement must
consume its entire line; multiline expressions are not supported.

Identifiers begin with a letter and continue with letters or digits. Unicode
letters are supported. Keywords cannot be used as names:
`let in if then else cons head tail isEmpty True False`.
Keyword prefixes are ordinary identifiers: `TrueValue`, `letx`, and `headway` are valid.

## Integers and operators

Integers are arbitrary precision. Arithmetic never silently wraps. Literals contain
ASCII decimal digits. Negative results are supported; write `0 - 5` to express a
negative number, since unary minus is not part of the grammar.

From highest to lowest precedence: function application, `*`, `+`/`-`, then `==`.
Application and binary operators associate to the left. Parentheses override precedence.

```haskell
1 + 2 * 3 -- 7
(1 + 2) * 3 -- 9
9223372036854775808 * 2 -- 18446744073709551616
```

`==` compares integers, Booleans, and lists structurally. Values of different types
are unequal. Functions are always unequal, including to themselves. Lists containing
functions follow that rule. This is language equality, not function extensionality.

## Functions and local bindings

Both `\` and `λ` introduce a lambda. Multiple parameters desugar into nested lambdas.

```haskell
add = \x y -> x + y
addFive = add 5
addFive 10 -- 15
let x = 5 in x * 2 -- 10
```

`let` evaluates its bound expression in the preceding environment; it is not recursive.
Named top-level definitions whose expression is a lambda can recurse:

```haskell
factorial = \n -> if n == 0 then 1 else n * factorial (n - 1)
factorial 5 -- 120
```

Other definitions evaluate against the preceding environment. `x = x` without an
existing `x` is an undefined-name error, not a self-referential value. Redefining
`x = x + 1` uses the preceding `x`. Existing closures retain captured bindings.
Mutually recursive definition groups and recursive local `let` are not supported.

## Booleans and lists

Conditions must be `True` or `False`. Only the selected branch is evaluated.
Lists may contain different value types and use commas between elements.

```haskell
if True then 1 else missing -- 1
[1, True, []]
cons 1 [2, 3] -- [1, 2, 3]
head [1, 2] -- 1
tail [1, 2] -- [2]
isEmpty [] -- True
```

`head []` and `tail []` produce runtime errors. List operations reject non-list
arguments. Built-in list operators are dedicated syntax, not first-class functions;
wrap one in a lambda when passing it as an argument.

## Errors, state, and limits

Evaluation stops at the first error, retaining preceding outputs and definitions.
A failed definition does not replace an existing binding. Diagnostics include a code,
a message, and a source span with one-based lines and UTF-16 columns (matching
Monaco), with an exclusive end.

The browser starts fresh for each Run. The REPL persists successful definitions
and offers `:env`, `:trace`, `:examples`, `:load <name>`, `:help`, and `:quit`.
`:load` runs a bundled example, not an arbitrary file.

Each submitted program defaults to 100,000 expression evaluations and a nesting
limit of 1,000. Parser nesting is also limited to 1,000. Tracing defaults to at most
2,000 entries of 512 characters each; truncation is reported and does not stop
execution. Library callers can configure evaluator limits and disable tracing.

HTTP evaluation also has a five-second deadline that includes response serialization,
a 65,536-character source limit, and a 256 KiB request-body limit. Limits are
practical guardrails for a learning tool, not process-level CPU or memory isolation.

The [shared examples](../examples/programs.json) contain expected results checked
by the test suite. Their syntax is authoritative for the playground examples too.
