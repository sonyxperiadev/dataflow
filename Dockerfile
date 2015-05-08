FROM haskell:7.8.4
MAINTAINER Oskar Wickström <oskar.wickstrom@gmail.com>

RUN cabal update

ADD . /usr/local/dataflow
WORKDIR /usr/local/dataflow
RUN cabal sandbox init
RUN cabal install --only-dependencies
RUN cabal configure
RUN cabal install

RUN ln -s /usr/local/dataflow/.cabal-sandbox/bin/dataflow /usr/bin/dataflow

WORKDIR /root
