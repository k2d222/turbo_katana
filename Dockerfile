FROM ocaml/opam:ubuntu-16.04 AS init-opam

RUN set -x && \
    git -C /home/opam/opam-repository pull origin master && opam update && \
    opam upgrade

RUN set -x && \
    : "Update and upgrade default packagee" && \
    sudo apt-get install && sudo apt-get upgrade && \
    sudo apt-get install bison flex -y

# --- #

FROM init-opam AS kat
COPY . .

RUN set -x && \
    : "Install related pacakges" && \
    opam install . --deps-only && \
    eval $(opam env) && \
    : "Build applications" && \
    dune build

RUN set -x && \
    : "Building Interp" && \
    cd interprete && sudo make clean && sudo make

#SSH §
EXPOSE 3000

LABEL org.opencontainers.image.source="https://github.com/OopsOverflow/turbo_katana"

# TODO : CMD [ "executable" ]