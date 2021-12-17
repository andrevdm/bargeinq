package = bargeinq
exe = bargeinq-exe
stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

all: build test lint

build: stack-build


setup:
	$(stack) setup
	$(stack) build --dependencies-only --test --no-run-tests
	$(stack) install hlint weeder

lint:
	hlint .
	weeder .

stack-check-nightly:
	$(stack) setup --resolver nightly
	$(stack) build --resolver nightly --pedantic --test

stack-build:
	$(stack) build $(package) --no-run-tests --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

stack-build-fast:
	$(stack) build $(package) --fast --no-run-tests --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

stack-build-watch:
	$(stack) build $(package) --fast --file-watch --no-run-tests --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

stack-test-watch:
	$(stack) test $(package) --fast --file-watch --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

stack-build-dirty:
	$(stack) build --ghc-options=-fforce-recomp $(package)

stack-build-profile:
	$(stack) --work-dir .stack-work-profiling --profile build

stack-run:
	$(stack) build $(package) --fast --no-run-tests --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS" && $(stack) exec -- $(package)-exe

stack-ghci:
	$(stack) ghci $(package):lib --ghci-options='-j8 +RTS -A128m -n2m -qg'

stack-test:
	$(stack) test $(package) --fast --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

stack-test-ghci:
	$(stack) ghci $(package):test:$(package)-tests --ghci-options='-j8 +RTS -A128m -n2m -qg'

stack-bench:
	$(stack) bench $(package)

stack-ghcid:
	$(stack) exec -- ghcid --lint -c "stack ghci $(package):lib --ghci-options='-fobject-code -fno-warn-unused-do-bind -j6 +RTS -A128m -n2m -qg' --main-is $(package):exe:$(package)-exe"

stack-ghcid-quiet:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --ghci-options='-fobject-code -fno-warn-unused-do-bind -fno-warn-unused-matches -fno-warn-unused-local-binds -fno-warn-unused-imports -j6 +RTS -A128m -n2m -qg' --main-is $(package):exe:$(package)-exe"

stack-ghcid-run:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --ghci-options='-fobject-code -fno-warn-unused-do-bind -j6 +RTS -A128m -n2m -qg' --main-is $(package):exe:$(package)-exe" --test=":main debug" -W

stack-ghcid-test:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --ghci-options='-fobject-code -fno-warn-unused-do-bind -j6 +RTS -A128m -n2m -qg' $(package):test:$(package)-test" --test="main" -W

dev-deps:
	stack install ghcid


cabal-run:
	cabal run $(exe)

cabal-build:
	cabal build $(package) --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

cabal-build-fast:
	cabal build $(package) --disable-optimisation --ghc-options "-O0 -j6 +RTS -A128m -n2m -qg -RTS"

cabal-ghcid:
	ghcid --lint -c "cabal repl --repl-options='-ignore-dot-ghci' --repl-options='-fobject-code' --repl-options='-fno-warn-unused-do-bind' --repl-options='-j6' "

cabal-test:
	cabal run --test-show-details=direct test:tests psql


.PHONY : stack-check-nightly stack-build stack-build-fast stack-build-watch stack-test-watch stack-build-dirty stack-build-profile stack-run stack-ghci stack-test stack-test-ghci stack-bench stack-ghcid stack-ghcid-quiet stack-ghcid-run stack-ghcid-test dev-deps cabal-run cabal-build cabal-build-fast cabal-ghcid cabal-test
