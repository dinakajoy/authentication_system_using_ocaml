install:
	opam switch create . ocaml-base-compiler.5.4.0
	eval $(opam env)
	opam update
	opam install . --deps-only -y

build:
	dune build

serve:
	dune exec server/server.exe
