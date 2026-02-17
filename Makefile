install:
	opam switch create . ocaml-base-compiler.5.4.0
	eval $(opam env)
	opam update
	opam install . --deps-only -y
	opam install cohttp cohttp-lwt-unix -y
	opam install dotenv -y

build:
	dune build

serve:
	dune exec server/server.exe
