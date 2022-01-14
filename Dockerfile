FROM ocaml/opam:alpine as base

ENV OCAML_VERSION 4.12.0

WORKDIR compil

RUN sudo apk update

# Install dependencies
ADD compil.opam .

RUN opam init -a -y --comp $OCAML_VERSION --disable-sandboxing 


RUN opam pin add -yn compil . && \
    opam depext compil && \
    opam install . && eval `opam config env` && \
    dune runtest


EXPOSE 3000
