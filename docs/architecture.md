# Architecture

## One language implementation

The React playground and CLI both use the Haskell program runner. The browser has
no alternate evaluator and never substitutes mock results when a request fails.

```text
source text → Parser → Statement / located Expr → Program → Evaluator → Value
                                                   │          │
                                                   └─ diagnostic, steps, trace
```

- **Parser** implements combinators directly. Its cursor tracks position and
  nesting. Failures record whether input was committed; lexical lookahead can
  backtrack, but a malformed committed expression cannot become a valid prefix.
  Alternative failures retain the furthest diagnostic. Each statement requires EOF.
- **Ast / Diagnostic** describe located syntax and structured failures. AST display
  strips location wrappers; diagnostics retain them.
- **Value / Evaluator** implement lexical environments, closures, exact integers,
  and call-by-value evaluation. Evaluation state carries strict counters and a
  bounded reverse trace that survives errors. Disabled tracing skips rendering.
- **Program / Lib** expose `runProgram :: EvalOptions -> Env -> String -> RunResult`.
  Statements run in order with one shared evaluation budget. Only successful
  statements update the environment; earlier results survive later failures.
- **Web** adapts a fresh program run to JSON. The timeout forces the serialized
  response, not just a lazy result constructor. Function environments never enter JSON.
- **Main** selects web or REPL mode and owns the REPL environment. **Examples** loads
  the same packaged JSON fixtures used by React and tests.

Top-level named lambda definitions tie a lazy captured environment containing their
own closure. Arbitrary expressions do not participate in that recursive binding.
Local `let` is nonrecursive. The core parser/evaluator/program modules have no HTTP
or JSON imports; transport representations belong to Web.

## HTTP interface

`POST /evaluate` accepts UTF-8 source as `text/plain` and returns:

```json
{
  "steps": [{"output": "42", "ast": "Add (Num 40) (Num 2)"}],
  "finalError": null,
  "diagnostic": null,
  "finalEnvironment": {},
  "traceLog": [],
  "traceTruncated": false,
  "evaluations": 3
}
```

`finalError` is a display string retained for compatibility. `diagnostic` is either
null or `{code, message, span: {start: {line, column}, end: {line, column}}}`.
The trace in the example is omitted for brevity; normal HTTP runs enable it.

Environment integers are always decimal **strings**, including inside lists.
Booleans remain JSON Booleans, lists remain arrays, and functions appear as
`"<closure>"`. This avoids JavaScript numeric rounding and recursive environment
serialization. This representation changes numeric environment values from v0.1.

Language, encoding, and evaluation-limit errors use the same JSON envelope with
HTTP 200; request-body rejection is handled by Scotty as HTTP 413. Clients must
inspect `finalError`, not only the HTTP status. A deadline or source-size failure
returns an empty result because a completed result is unavailable.

## Browser and deployment

React validates the response at its network boundary, then displays outputs,
ASTs, traces, environment values, and Monaco markers. The client deadline includes
body parsing. Every new run clears old results. Network failure is visible and
retryable. Source sharing uses UTF-8 and URL-encoded base64.

Haskell serves `web-client/dist` with explicit asset MIME types. Development uses
a Vite `/evaluate` proxy to the same server. Docker builds React and Haskell, then
runs one service. There is no server-side session store or shared global environment.

## Verification

HUnit covers language behavior, diagnostics, state, budgets, and HTTP serialization.
QuickCheck checks arithmetic and whitespace invariants. Shared example fixtures
are checked against their expected output. Node tests cover the frontend boundary,
including a stalled body; Playwright exercises the actual backend and browser.
The test runner exits nonzero on failed assertions or properties.
