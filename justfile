live:
	fd . app | entr -rc cabal run

run: 
	cabal run
