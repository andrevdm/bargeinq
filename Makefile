package = bargeinq
exe = bargeinq-exe

build: cabal-build

lint:
	hlint .
	weeder .

cabal-run:
	cabal run $(exe)

cabal-run-fast:
	cabal run $(exe) --ghc-options "-O0 -j6 +RTS -A128m -n2m -qg -RTS"

cabal-build:
	cabal build $(package) --ghc-options "-j6 +RTS -A128m -n2m -qg -RTS"

cabal-build-fast:
	cabal build $(package) --disable-optimisation --ghc-options "-O0 -j6 +RTS -A128m -n2m -qg -RTS"

cabal-ghcid:
	ghcid --lint -c "cabal repl --repl-options='-ignore-dot-ghci' --repl-options='-fobject-code' --repl-options='-fno-warn-unused-do-bind' --repl-options='-j6' "

cabal-test:
	cabal run --test-show-details=direct test:tests psql


.PHONY : cabal-build cabal-build-fast cabal-ghcid cabal-test
