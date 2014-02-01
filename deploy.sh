#!/bin/bash

# Deployment script
# Copies the relevant scripts to $HOME/bin and sets their permissions

BIN_DIR=$HOME/bin
BuildSuccess=0

if [ ! -d $BIN_DIR ]
then
  echo "$BIN_DIR does not exist. Creating it"
  mkdir -v $BIN_DIR
fi

case "$1" in
  all)
    ./build.sh git-grep-filter
    BuildSuccess=$?
    if [ $BuildSuccess -eq 0 ]
    then
      cp dist/build/git-grep-filter/git-grep-filter $BIN_DIR/ggf
      cp git-grep-blame.rb $BIN_DIR/ggb
      cp git_utils.rb $BIN_DIR/
      cp setup_cabal.sh $BIN_DIR/setup_cabal
      cp setup_haskell_sandboxes.rb $BIN_DIR/setup_haskell_sandboxes
      cd $BIN_DIR
      chmod 700 ggb ggf setup_cabal setup_haskell_sandboxes
    else
      echo "Deploy failed"
    fi
    ;;

  git-grep-filter)
    ./build.sh git-grep-filter
    if [ $? -eq 0 ]
    then
      cp dist/build/git-grep-filter/git-grep-filter $BIN_DIR/ggf
      chmod 700 $BIN_DIR/ggf
    fi
    ;;

  git-switch-branch)
    ./build.sh git-switch-branch
    if [ $? -eq 0 ]
    then
      cp dist/build/git-switch-branch/git-switch-branch $BIN_DIR/gsb
      chmod 700 $BIN_DIR/gsb
    fi
    ;;

  scpi)
    cp scpi.sh $BIN_DIR/scpi
    chmod 700 $BIN_DIR/scpi
	;;

  *)
    echo "$0: Unknown target to deploy"
    exit 1
    ;;
esac
