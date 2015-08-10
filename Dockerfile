FROM ubuntu:trusty
MAINTAINER Oskar Wickström <oskar.wickstrom@gmail.com>

# HASKELL

RUN echo 'deb http://ppa.launchpad.net/hvr/ghc/ubuntu trusty main' > /etc/apt/sources.list.d/ghc.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys F6F88286 && \
    apt-get update && \
    apt-get install -y --no-install-recommends cabal-install-1.20 ghc-7.8.4 happy-1.19.4 alex-3.1.3 \
            zlib1g-dev libtinfo-dev libsqlite3-0 libsqlite3-dev && \
    rm -rf /var/lib/apt/lists/*

ENV PATH /root/.cabal/bin:/opt/cabal/1.20/bin:/opt/ghc/7.8.4/bin:/opt/happy/1.19.4/bin:/opt/alex/3.1.3/bin:$PATH

## DATAFLOW

ENV LANG C.UTF-8

ADD . /usr/local/dataflow
WORKDIR /usr/local/dataflow
RUN cabal update && \
    cabal sandbox init && \
    cabal install --only-dependencies && \
    cabal configure && \
    cabal install

RUN ln -s /usr/local/dataflow/.cabal-sandbox/bin/dataflow /usr/bin/dataflow

WORKDIR /root
