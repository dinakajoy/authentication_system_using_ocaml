install:
	opam switch create . ocaml-base-compiler.5.4.0
	eval $(opam env)
	opam update
	opam install . --deps-only -y
# 	opam install cohttp-lwt-unix -y

build:
	dune build

serve:
	set -a && . .env && set +a && dune exec server/server.exe
