all: exe
.PHONY: all exe

exe:
	cabal install --overwrite-policy=always

