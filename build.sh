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

  git-commit-time)
    cabal build git-commit-time
    ;;

  scpi)
    ;;

  ssh-rm-host)
    ;;

  *)
    echo "unknown build target"
    exit 1
    ;;
esac
