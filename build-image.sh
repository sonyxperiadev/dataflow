#!/bin/bash

VERSION=`grep '^version:' dataflow.cabal | sed -Ee 's/^version\:(.+)$/\1/' | xargs`
echo "Building version $VERSION"
docker build -t owickstrom/dataflow:$VERSION .
