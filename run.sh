#!/bin/bash
cabal install
./dist/build/elfeckcom/elfeckcom &
while inotifywait -e modify -r ./src/; do
    pkill elfeckcom
    cabal install
    ./dist/build/elfeckcom/elfeckcom &
done
