# L Language Reference

## Syntax

L Language uses a minimalist syntax heavily inspired by Haskell and Lambda Calculus.

### Definitions

Define variables or functions using the `=` operator using the line start.

```haskell
x = 10
double = \x -> x * 2
```

### Anonymous Functions (Lambdas)

Functions are defined using the `\` (lambda) character, followed by arguments, `->`, and the body.

```haskell
\x -> x + 1
\x y -> x * y  -- Multi-argument lambda
```

### Conditionals

Standard `if-then-else` expressions.

```haskell
if x == 0 then 1 else 2
```

### Lists

L supports basic linked lists.

- `[]` : Empty list (conceptually). In L, we often check emptiness.
- `cons head tail` : Construct a list.
- `head list` : Get the first element.
- `tail list` : Get the rest of the list.
- `isEmpty list` : Check if a list is empty.

_(Note: The current syntax primarily uses `cons`, `head`, `tail` built-ins rather than `[]` literal syntax for construction in all contexts, though usage patterns may evolve.)_

### Built-in Operations

- **Arithmetic**: `+`, `-`, `*`
- **Comparison**: `==`
- **List Ops**: `cons`, `head`, `tail`, `isEmpty`

## Examples

### Factorial

```haskell
factorial = \n -> if n == 0 then 1 else n * factorial (n - 1)
factorial 5
-- Result: 120
```

### Fibonacci

```haskell
fib = \n -> if n == 0 then 0 else if n == 1 then 1 else fib (n - 1) + fib (n - 2)
fib 10
-- Result: 55
```

### Map

Creating a higher-order `map` function to apply a function to a list.

```haskell
map = \f xs -> if isEmpty xs then [] else cons (f (head xs)) (map f (tail xs))

-- Usage
let myList = cons 1 (cons 2 (cons 3 []))
in map (\x -> x * 2) myList
```

_(Requires `[]` literal support or definition of `nil` in environment)_

### Recursive Let

You can use `let` for local bindings. Recursive `let` is supported in definitions.

```haskell
let x = 5 in x * 2
```
