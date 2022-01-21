FROM ocaml/opam:alpine AS init-opam

RUN set -x && \
    git -C /home/opam/opam-repository pull origin master && opam update && \
    opam upgrade

RUN set -x && \
    : "Update and upgrade default packagee" && \
    sudo apk update && sudo apk upgrade && \
    sudo apk add linux-headers

# --- #

FROM init-opam AS kat
COPY . .
RUN set -x && \
    : "Install related pacakges" && \
    opam install . --deps-only && \
    eval $(opam env) && \
    : "Build applications" && \
    dune build

#SSH §
EXPOSE 3000

# TODO : CMD [ "executable" ] 