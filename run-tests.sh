#!/usr/bin/env bash

cabal exec runhaskell -- -isrc -itest test/Spec.hs
