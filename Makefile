all: test

test: test.ml rules.ml lexer.mll
	ocamlbuild -use-ocamlfind test.native && ln -sf test.native test

clean:
	rm _build/* ; rm *.byte ; rm *.native

.PHONY: parse test all
