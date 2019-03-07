parse: parse.ml lexer.mll
	ocamlbuild -use-ocamlfind parse.byte
