#!/bin/bash

# Deployment script
# Copies the relevant scripts to $HOME/bin and sets their permissions

BIN_DIR=$HOME/bin

cp git-grep-blame.rb $BIN_DIR/ggb

cd $BIN_DIR
chmod 700 ggb
