FROM ocaml/opam:ubuntu-16.04 as base

ENV OCAML_VERSION 4.12.0

WORKDIR /compil

RUN sudo apt-get update -y && sudo apt-get upgrade -y


ADD compil.opam .

RUN opam init -a -y --comp $OCAML_VERSION --disable-sandboxing 


RUN opam pin add -yn compil . && \
    opam depext compil && \
    opam install . && sudo -s eval `opam config env` && opam install dune && \
    dune runtest


EXPOSE 3000
