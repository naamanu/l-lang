# Development guide

Use Stack (GHC 9.8.4 is pinned) and Node.js 22. Clone the project and build:

```sh
git clone https://github.com/naamanu/l-lang.git
cd l-lang
stack build
npm --prefix web-client ci
npm --prefix web-client run build
stack run -- -w
```

Open http://localhost:3000. For a CLI session use `stack run -- -r`.

For frontend development, leave Haskell running and use another terminal:

```sh
npm --prefix web-client run dev
```

Vite proxies `/evaluate` to Haskell at port 3000. Frontend changes refresh at
http://localhost:5173. All evaluation still happens in Haskell.

`PORT` selects the Haskell port (default 3000). `L_LANG_ASSETS` selects the built
React directory (default `web-client/dist`). Run commands from the repository root.
The development proxy uses port 3000. Integration tests use a separate port, 3107.

## Tests

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

Playwright starts and stops a server automatically. Port 3107 must be free (`L_LANG_TEST_PORT` overrides it).
`PLAYWRIGHT_CHANNEL=chrome npm test` uses an installed Chrome instead of Playwright's
Chromium. `L_LANG_SERVER_COMMAND` overrides the command used to start the test server.

To prove that failed assertions fail the test command, run this deliberately failing
check; **exit status 1 is expected**:

```sh
stack test --test-arguments=--prove-failure
```

The examples live in `examples/programs.json`, with stable IDs, source code, and
expected final output. The REPL, browser, and tests all consume these fixtures.

## Containers

```sh
docker compose up --build
```

This builds both layers and serves the application at http://localhost:3000.
Docker and CI use the pinned compiler. Node dependencies use the committed lockfile.
