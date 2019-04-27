all: test

test: test.ml rules.ml lexer.mll
	ocamlbuild -use-ocamlfind test.byte

clean:
	rm _build/*

.PHONY: parse test all
