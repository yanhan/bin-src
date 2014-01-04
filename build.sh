#!/bin/bash

case "$1" in
  git-grep-filter)
    cabal build git-grep-filter
    ;;

  *)
    echo "unknown build target"
    exit 1
    ;;
esac
