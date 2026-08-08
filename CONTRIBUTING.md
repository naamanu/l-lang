# Contributing to L

Thanks for your interest in contributing.

L is a minimalist untyped functional language inspired by the lambda calculus. It is a
small codebase on purpose — six modules — so most contributions touch one or two files.

## Prerequisites

- [Stack](https://docs.haskellstack.org/) (it installs the right GHC for you)
- `alex` and `happy` — declared as build tools, so Stack fetches them

## Getting started

```bash
git clone https://github.com/your-username/l-lang.git
cd l-lang
stack build
stack test
```

Run the REPL, or the web playground on http://localhost:3000:

```bash
stack run              # REPL
stack run -- -w        # web playground
```

`make build`, `make run`, and `make clean` wrap the same commands. Docker is also
supported via `docker-compose.yml`.

## Project structure

```
src/Parser.hs      # Parsec-based parser
src/Ast.hs         # Syntax tree
src/Evaluator.hs   # Evaluation
src/Value.hs       # Runtime values
src/Lib.hs         # Library entry point, Scotty web server
app/Main.hs        # CLI entry point (REPL / -w playground)
test/Spec.hs       # hspec + QuickCheck suite
web-client/        # Playground front end
```

## Before you open a PR

```bash
stack build
stack test
```

`ghc-options` enables `-Wall` plus a strict set of extra warnings
(`-Wincomplete-uni-patterns`, `-Wmissing-export-lists`, and others). Please don't add
new warnings; a warning-free build is the bar.

## Making changes

1. Branch off `main`: `git checkout -b feat/your-change`
2. Add a test in `test/Spec.hs`. The suite has hspec, QuickCheck, and HUnit available —
   QuickCheck properties are especially welcome for parser and evaluator work.
3. If you add syntax, add an example to the README and to `test.l`.
4. Note behaviour changes in `CHANGELOG.md`.
5. Open a PR against `main`, linking any related issue.

## Reporting issues

Please include:

- The L program that reproduces it (as small as you can make it)
- Whether it happened in the REPL or the web playground
- Expected vs actual result

## License

By contributing, you agree that your contributions will be licensed under the
BSD-3-Clause License.
