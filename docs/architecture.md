# Architecture Guide

The L Language project consists of a Haskell-based interpreter and a web interface.

## Implementation Overview

### 1. Core Interpreter (`src/`)

The core logic resides in `src/` and is pure Haskell.

- **`Parser.hs`**:

  - Uses monadic parser combinators (custom implementation similar to Parsec/Megaparsec).
  - Parses raw strings into an Abstract Syntax Tree (AST).
  - Key types: `Parser a`, `Expr` (AST).

- **`Ast.hs`**:

  - Defines the `Expr` data type representing language constructs (Var, App, Lam, Let, If, etc.).

- **`Evaluator.hs`**:
  - Implements the evaluation logic using a standard substitution model or environment-based evaluation.
  - **Traceable**: The `eval` function returns not just the result, but a `TraceLog` (list of strings) documenting every step.
  - **Environment**: Manages variable scopes (`Env` map).
  - **Values**: Defines runtime values `Value` (Closures, Numbers, Booleans).

### 2. Application Layer (`app/`)

- **`Main.hs`**: The entry point.
  - Parses command line arguments (`-r` for REPL, `-w` for Web).
  - **CLI REPL**: Handles standard input/output, maintains loop state.
  - **Web Server**: Uses **Scotty** (a Haskell web framework) to serve:
    - Static files (`static/index.html`, `static/script.js`).
    - API endpoint `POST /evaluate` which accepts code, runs the interpreter, and returns JSON containing results, AST, and trace logs.

### 3. Frontend (`static/`)

A lightweight, vanilla JS + Tailwind CSS frontend served by the Haskell app.

- **`index.html`**: The single-page application structure.
- **`script.js`**: Fetches evaluation results from the backend and renders them to the DOM.

## Data Flow (Web)

1.  User types code in Browser.
2.  `POST /evaluate` -> Haskell Server.
3.  Server parses code (lines).
4.  Server evaluates code sequentially, accumulating environment changes.
5.  Server returns JSON:
    ```json
    {
      "finalEnvironment": {...},
      "steps": [{ "output": "...", "ast": "..." }],
      "traceLog": ["Eval ...", "Apply ..."],
      "finalError": null
    }
    ```
6.  Frontend renders JSON data to the UI.
