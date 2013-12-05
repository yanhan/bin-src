#!/bin/bash

# Deployment script
# Copies the relevant scripts to $HOME/bin and sets their permissions

BIN_DIR=$HOME/bin

cp git-grep-blame.rb $BIN_DIR/ggb
cp git_utils.rb $BIN_DIR/

cd $BIN_DIR
chmod 700 ggb
