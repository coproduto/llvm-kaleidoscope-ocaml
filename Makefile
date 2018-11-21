all:
	ocamlbuild -use-ocamlfind -package llvm toy.native
