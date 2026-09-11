# Changelog

## 0.2.0.0 — Unreleased

- Share program execution between the REPL and HTTP server; isolate every browser run.
- Preserve earlier successful statements on failure; restrict recursive binding to named lambdas.
- Use exact integers and decimal-string JSON environment values (an API representation change).
- Fix keyword boundaries and reserved binders; add comments and positioned diagnostics.
- Bound evaluation depth, steps, trace output, request size, and HTTP evaluation duration.
- Serve one React playground with a locally bundled editor; remove mock and duplicate interfaces.
- Use shared executable examples and add semantic, property, HTTP, and browser tests with CI.
- Fix failing-test exit status and remove blanket warning suppression and unused dependencies.

## 0.1.0.0

Initial interpreter, REPL, and playground.
