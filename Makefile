.PHONY: run build test playground clean
run:
	stack run -- ${ARGS}
build:
	stack build --ghc-options=-Werror
test:
	stack test --ghc-options=-Werror
	npm --prefix web-client run lint
	npm --prefix web-client run test:unit
playground:
	npm --prefix web-client ci
	npm --prefix web-client run build
	stack run -- -w
clean:
	stack clean
