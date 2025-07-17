FROM ocaml/opam:debian-12-ocaml-5.1 as build
WORKDIR /build

# Install dependencies.
RUN sudo apt-get update && sudo apt-get install -y \
  libev-dev \
  libpq-dev \
  libssl-dev \
  libgmp-dev \
  pkg-config \
  libpcre3-dev \
  libpcre3

ADD . .

# Fix permissions so opam user can build
RUN sudo chown -R opam:opam /build

RUN opam install --deps-only . --yes

# Default command
CMD ["opam", "exec", "--", "dune", "exec", "server"]
