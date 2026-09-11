FROM node:22-bookworm AS frontend
WORKDIR /build/web-client
COPY web-client/package*.json ./
RUN npm ci
COPY web-client/ ./
COPY examples/ /build/examples/
RUN npm run build

FROM haskell:9.8.4 AS backend
WORKDIR /build
COPY stack.yaml stack.yaml.lock package.yaml l-lang.cabal ./
RUN stack setup && stack build --only-dependencies
COPY src/ src/
COPY app/ app/
COPY test/ test/
COPY examples/ examples/
COPY README.md CHANGELOG.md LICENSE ./
COPY docs/ docs/
RUN stack build --ghc-options=-Werror --copy-bins --local-bin-path /out

FROM haskell:9.8.4
WORKDIR /app
ENV l_lang_datadir=/app
COPY examples/ /app/examples/
COPY --from=backend /out/l-lang-exe /usr/local/bin/l-lang-exe
COPY --from=frontend /build/web-client/dist /app/web-client/dist
EXPOSE 3000
CMD ["l-lang-exe", "-w"]
