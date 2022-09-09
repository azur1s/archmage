all: exe std

exe:
	cabal install --overwrite-policy=always

std:
	mkdir -p $(HOME)/.cyxstd/
	cp std/*.cyx $(HOME)/.cyxstd/