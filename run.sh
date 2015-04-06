#!/bin/bash
cabal build
cabal run &
while inotifywait -e modify -r ./src/; do
    pkill elfeckcom
    cabal build
    cabal run &
done
