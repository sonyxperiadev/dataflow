#!/bin/bash

if [ ! `command -v nodemon` ]; then
  printf "\n\033[31;1mYou need to install nodemon globally to run watch-tests.sh!\033[0m\n"
  printf "\n  npm install -g nodemon\n\n"
  exit 1
fi

nodemon --watch src --watch test -e hs --exec ./run-tests.sh
