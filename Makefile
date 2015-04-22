boot:
	cabal build

sandbox:
	cabal sandbox init
	cabal install --only-dependencies --enable-tests

clean:
	cabal clean
	cabal sandbox delete
