FROM ubuntu:15.10
MAINTAINER Oskar Wickström <oskar.wickstrom@gmail.com>

# HASKELL

RUN apt-get update && \
    apt-get install -y haskell-platform

## DATAFLOW

ENV LANG C.UTF-8

ADD . /usr/local/dataflow
WORKDIR /usr/local/dataflow

RUN ghc-pkg unregister HTTP
RUN ghc-pkg unregister vector
RUN ghc-pkg unregister QuickCheck
RUN ghc-pkg unregister tf-random

RUN cabal update && \
    cabal sandbox init && \
    cabal install --only-dependencies && \
    cabal configure && \
    cabal install

RUN ln -s /usr/local/dataflow/.cabal-sandbox/bin/dataflow /usr/bin/dataflow

WORKDIR /root
