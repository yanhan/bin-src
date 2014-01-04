#!/bin/bash

# Deployment script
# Copies the relevant scripts to $HOME/bin and sets their permissions

BIN_DIR=$HOME/bin

case "$1" in
  all)
    cp git-grep-blame.rb $BIN_DIR/ggb
    cp git-grep-filter.rb $BIN_DIR/ggf
    cp git_utils.rb $BIN_DIR/
    cp setup_cabal.sh $BIN_DIR/setup_cabal
    cp setup_haskell_sandboxes.rb $BIN_DIR/setup_haskell_sandboxes
    cd $BIN_DIR
    chmod 700 ggb ggf setup_cabal setup_haskell_sandboxes
    ;;

  *)
    echo "$0: Unknown target to deploy"
    exit 1
    ;;
esac
