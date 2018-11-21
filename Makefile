all:
	ocamlbuild -use-ocamlfind -package llvm -package llvm.analysis toy.native
