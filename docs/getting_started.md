# Getting Started with L Language

## Prerequisites

To build and run the L Language interpreter from source, you need:

- **GHC** (Glasgow Haskell Compiler) & **Stack**: The standard Haskell build toolchain.
  - [Install Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)

## Installation

1.  Clone the repository:

    ```bash
    git clone https://github.com/claeusdev/l-lang
    cd l-lang
    ```

2.  Build the project:
    ```bash
    stack build
    ```

## Usage

The L Language interpreter supports two main modes:

### 1. Web Playground (Recommended)

Starts a local web server with a rich graphical interface (GUI).

```bash
stack run -- -w
```

- Access the playground at **[http://localhost:3000](http://localhost:3000)**.
- **Features**:
  - Split-pane editor and output.
  - Visual execution trace.
  - Environment inspector.
  - Pre-loaded syntax highlighting (monospaced editor).

### 2. CLI REPL

Starts an interactive Read-Eval-Print Loop in your terminal.

```bash
stack run -- -r
```

- **Commands**:
  - `:help` or `:?` - Show available commands.
  - `:trace` - Toggle verbose evaluation tracing (ON by default).
  - `:env` - Print the current environment (defined variables).
  - `:load <name>` - Load a built-in example (e.g., `:load factorial`).
  - `:quit` - Exit the REPL.

## Running Tests

To look at tests:

```bash
stack test
```

## Docker (Alternative)

You can also run the project using Docker Compose if you prefer not to install Haskell locally.

```bash
docker compose up --build
```

This will start the Haskell backend (port 3000) and the React web client (port 5173).
