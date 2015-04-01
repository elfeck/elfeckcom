#!/bin/bash
cabal install
./dist/build/elfeckcom/elfeckcom &
while inotifywait -e modify . ./static/header.svg; do
    pkill elfeckcom
    cabal install
    ./dist/build/elfeckcom/elfeckcom &
done
