FROM haskell:8.8.3 AS deps
RUN mkdir /opt/deps/
WORKDIR /opt/deps/
COPY stack.yaml crossword-hs.cabal stack.yaml.lock /opt/deps/
RUN stack build server --system-ghc --dependencies-only

FROM haskell:8.8.3 AS build
WORKDIR /opt/build
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build
RUN stack build server --system-ghc --copy-bins

# haskell:8 is based on debian:buster so this should save some download time.
FROM debian:buster
RUN mkdir -p /opt/app
WORKDIR /opt/app

COPY --from=build /root/.local/bin/server .
CMD ["/opt/app/server"]
