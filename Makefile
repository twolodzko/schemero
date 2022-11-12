MAKEFLAGS += --no-print-directory
MAKEFLAGS += --no-builtin-rules

.PHONY: test
test: unit_test integration_test clean ## Run tests

.PHONY: unit_test
unit_test: ## Run unit tests
	rebar3 eunit --dir=src

.PHONY: integration_test
integration_test: clean build ## Run integration tests
	@ printf "\nRunning integration tests:\n"
	./schemero examples/simple.scm
	./schemero examples/the-little-schemer/run-all.scm

.PHONY: repl
repl: build ## Start REPL
	@ chmod +x ./schemero
	@ ./schemero

ebin/: src/*
	@ mkdir -p ebin/
	erlc -o ebin src/*.erl

.PHONY: check_types
check_types: ## Type check
	@ erlc +debug_info -o ebin src/*.erl
	@ dialyzer --quiet --build_plt -r ebin/
	dialyzer --quiet --src src/

.PHONY: build
build: ebin/ ## Build it

lines: ## Count the number of code lines
	@ find . -type f \( -name "*.erl" -not -path "./_build/*" -not -path "./ebin/*" -not -name "*_tests.erl" -not -path "./.github/*" \) -exec cat {} \; | grep . | wc -l

.PHONY: clean
clean: ## Cleanup
	@ rm -rf _build/
	@ find . -type f -name "*.beam" -delete
	@ rm -rf *.crashdump *.dump *.lock
	@ rm -rf ebin/

help: ## Display this help
	@ grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
