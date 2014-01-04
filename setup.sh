#!/bin/bash

# Setup script for this folder
# Please execute this script once before building / deploying anything

if ! [ -d .cabal-sandbox ]
then
  cabal sandbox init
  cabal configure
  cabal install --only-dependencies
fi
