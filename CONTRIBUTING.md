# Contributing to L

L is a small language implementation. Keep changes explainable through a language
rule, an executable example, and a test of observable behavior.

Read the [language reference](docs/language_reference.md) and
[architecture](docs/architecture.md) before changing syntax or evaluation.

## Development

Use Stack and Node.js 22. See the [development guide](docs/getting_started.md)
for setup and the complete verification commands. CI checks Haskell with warnings
as errors, frontend lint/build, unit tests, and real-server browser tests.

`package.yaml` is the Haskell manifest source. Stack regenerates `l-lang.cabal`;
commit both when changing the manifest. Keep testing libraries in the test component.
Do not suppress warnings globally or introduce a second interpreter in the UI.

For a behavior change:

1. Add a regression in `test/Spec.hs` or the relevant frontend tests.
2. Update `examples/programs.json` when an example should demonstrate the change.
3. Update the language reference and changelog.
4. Run the affected checks, then the full integration suite for interface changes.

For parser changes, test valid programs as well as malformed input, keyword
boundaries, source positions, and full consumption. For evaluator changes, include
scope, evaluation order, recursion, and error behavior. Test observable outcomes
rather than exact incidental trace formatting.

Open a pull request with the problem, resulting behavior, and verification results.
Do not include build caches, node_modules, or generated frontend assets.

Contributions are licensed under BSD-3-Clause.
