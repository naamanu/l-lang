# L playground

React and Monaco provide the editor; Haskell is the only evaluator. Monaco and its
worker are bundled locally. The editor loads as a separate chunk.

Use Node.js 22. From the repository root:

```sh
npm --prefix web-client ci
npm --prefix web-client run build
stack run -- -w
```

Open http://localhost:3000. For development, run `npm --prefix web-client run dev`
in a second terminal; Vite proxies evaluation to Haskell.

Run `npm run lint`, `npm run test:unit`, and `npm run build` here. Browser tests use
`npx playwright install chromium` followed by `npm test`; they start the actual
Haskell server. See [the development guide](../docs/getting_started.md).

Numbers in environment responses are decimal strings, never JavaScript numbers.
Every Run starts fresh. A server outage shows an error, with no offline fallback.
The example fixtures are shared with Haskell at `../examples/programs.json`.
