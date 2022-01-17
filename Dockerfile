FROM ocaml/opam:ubuntu-16.04 as base

ENV OCAML_VERSION 4.12.0

WORKDIR /compil

RUN sudo apt-get update -y && sudo apt-get upgrade -y


ADD . .

RUN set -x && \
    : "Install related pacakges" && \
    opam install . --deps-only --locked && \
    opam update && eval $(opam env) 

RUN opam pin add -yn compil . && \
    opam depext compil && \
    opam install . && sudo -s eval `opam env` && \
    dune runtest


EXPOSE 3000
