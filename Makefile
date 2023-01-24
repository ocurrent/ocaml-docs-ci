.DEFAULT_GOAL := all

.PHONY: all
all:
	opam exec -- dune build --root . @install

.PHONY: deps
deps: ## Install development dependencies
	opam pin add -yn current_docker.dev "./vendor/ocurrent" && \
	opam pin add -yn current_github.dev "./vendor/ocurrent" && \
	opam pin add -yn current_git.dev "./vendor/ocurrent" && \
	opam pin add -yn current.dev "./vendor/ocurrent" && \
	opam pin add -yn current_rpc.dev "./vendor/ocurrent" && \
	opam pin add -yn current_slack.dev "./vendor/ocurrent" && \
	opam pin add -yn current_web.dev "./vendor/ocurrent"
	opam install -y dune-release ocamlformat utop ocaml-lsp-server obuilder-spec
	opam install --deps-only --with-test --with-doc -y .

.PHONY: create_switch
create_switch:
	opam switch create . 4.12.0 --no-install

.PHONY: switch
switch: create_switch deps ## Create an opam switch and install development dependencies

.PHONY: lock
lock: ## Generate a lock file
	opam lock -y .

.PHONY: build
build: ## Build the project, including non installable libraries and executables
	opam exec -- dune build --root .

.PHONY: install
install: all ## Install the packages on the system
	opam exec -- dune install --root .

.PHONY: start
start: all ## Run the produced executable
	opam exec -- dune exec --root . src/ocaml_docs_ci.exe

.PHONY: test
test: ## Run the unit tests
	opam exec -- dune build --root . @test/runtest -f

.PHONY: clean
clean: ## Clean build artifacts and other generated files
	opam exec -- dune clean --root .

.PHONY: doc
doc: ## Generate odoc documentation
	opam exec -- dune build --root . @doc

.PHONY: fmt
fmt: ## Format the codebase with ocamlformat
	opam exec -- dune build --root . --auto-promote @fmt

.PHONY: utop
utop: ## Run a REPL and link with the project's libraries
	opam exec -- dune utop --root . src -- -implicit-bindings
