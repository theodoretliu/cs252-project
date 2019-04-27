all: parse test

parse: parse.ml lexer.mll
	ocamlbuild -use-ocamlfind parse.byte

test: test.ml parse.ml lexer.mll
	ocamlbuild -use-ocamlfind test.byte

clean:
	rm _build/*

.PHONY: parse test all
