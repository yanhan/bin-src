#!/bin/bash

case "$1" in
  all)
    cabal build git-grep-filter
    cabal build git-switch-branch
    ;;

  git-grep-filter | ggf)
    cabal build git-grep-filter
    ;;

  git-switch-branch | gsb)
    cabal build git-switch-branch
    ;;

  scpi)
    ;;

  *)
    echo "unknown build target"
    exit 1
    ;;
esac
