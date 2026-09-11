# L

L is a small, untyped functional language implemented in Haskell. Its handwritten
parser and environment-based evaluator make lexical closures, currying, and
recursion visible through bounded evaluation traces.

```haskell
factorial = \n -> if n == 0 then 1 else n * factorial (n - 1)
factorial 5
-- 120
```

## Run it

Install [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
The repository pins GHC 9.8.4 through LTS 23.10.

```sh
stack build
stack run -- -r
```

For the React playground, also install Node.js 22 and build its assets:

```sh
npm --prefix web-client ci
npm --prefix web-client run build
stack run -- -w
```

Open **http://localhost:3000**. Haskell serves both the playground and its API.
Every browser Run evaluates the complete editor contents from a fresh environment.
The CLI REPL keeps definitions between commands. Monaco is bundled locally.

```sh
docker compose up --build
```

Docker runs the same playground and interpreter at port 3000.

## What the language supports

- Exact integers; `+`, `-`, `*`, and structural equality with `==`.
- Lexically scoped functions, multiple arguments through currying, and recursive
  named lambda definitions.
- Nonrecursive `let`, Boolean conditionals, lists, and list operations.
- Source-position diagnostics and optional, bounded evaluation traces.

Statements occupy one line; blank lines and `--` comments are allowed. There is
no type checker, compiler, division, list comprehension, or alternate browser
interpreter. See the [language reference](docs/language_reference.md).

## Check it

```sh
stack build --ghc-options=-Werror
stack test --ghc-options=-Werror
npm --prefix web-client run lint
npm --prefix web-client run test:unit
npm --prefix web-client run build
cd web-client
npx playwright install chromium
npm test
```

Browser tests start their own Haskell server; leave port 3107 available.
[Executable examples](examples/programs.json) are shared by the playground and
Haskell tests. CI checks the examples, semantics, HTTP isolation, and browser flows.

Read the [architecture](docs/architecture.md), [development guide](docs/getting_started.md),
and [contribution guide](CONTRIBUTING.md) for implementation details.
