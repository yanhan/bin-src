#!/bin/bash

# Deployment script
# Copies the relevant scripts to $HOME/bin and sets their permissions

BIN_DIR=$HOME/bin

cp git-grep-blame.rb $BIN_DIR/ggb
cp git-grep-filter.rb $BIN_DIR/ggf
cp git_utils.rb $BIN_DIR/
cp setup_cabal.sh $BIN_DIR/setup_cabal

cd $BIN_DIR
chmod 700 ggb ggf setup_cabal
