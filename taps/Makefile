.PHONY: test

build:
	dune build

test:
	OCAMLRUNPARAM=b dune exec test/taps.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

doc:
	dune build @doc
	