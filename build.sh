#!/bin/bash

case "$1" in
  git-grep-filter)
    cabal build git-grep-filter
    ;;

  git-switch-branch)
    cabal build git-switch-branch
    ;;

  scpi)
    ;;

  *)
    echo "unknown build target"
    exit 1
    ;;
esac
