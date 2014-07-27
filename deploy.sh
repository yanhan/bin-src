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
    ./build.sh all
    BuildSuccess=$?
    if [ $BuildSuccess -eq 0 ]
    then
      cp cljs-repl.sh $BIN_DIR/cljs-repl
      cp dist/build/git-grep-filter/git-grep-filter $BIN_DIR/ggf
      cp dist/build/git-switch-branch/git-switch-branch $BIN_DIR/gsb
      cp git-grep-blame.rb $BIN_DIR/ggb
      cp git_utils.rb $BIN_DIR/
      cp setup_cabal.sh $BIN_DIR/setup_cabal
      cp setup_haskell_sandboxes.rb $BIN_DIR/setup_haskell_sandboxes
      cp ssh-rm-host.sh $BIN_DIR/ssh-rm-host
      cd $BIN_DIR
      chmod 700 cljs-repl ggb gsb ggf setup_cabal setup_haskell_sandboxes \
        ssh-rm-host
    else
      echo "Deploy failed"
    fi
    ;;

  cljs-repl)
    cp cljs-repl.sh $BIN_DIR/cljs-repl
    chmod 700 $BIN_DIR/cljs-repl
    ;;

  git-grep-filter | ggf)
    ./build.sh git-grep-filter
    if [ $? -eq 0 ]
    then
      cp dist/build/git-grep-filter/git-grep-filter $BIN_DIR/ggf
      chmod 700 $BIN_DIR/ggf
    fi
    ;;

  git-switch-branch | gsb)
    ./build.sh git-switch-branch
    if [ $? -eq 0 ]
    then
      cp dist/build/git-switch-branch/git-switch-branch $BIN_DIR/gsb
      chmod 700 $BIN_DIR/gsb
    fi
    ;;

  git-commit-time)
    ./build.sh git-commit-time
    if [ $? -eq 0 ]
    then
      cp dist/build/git-commit-time/git-commit-time $BIN_DIR/git-commit-time
      chmod 700 $BIN_DIR/git-commit-time
    fi
    ;;

  scpi)
    cp scpi.sh $BIN_DIR/scpi
    chmod 700 $BIN_DIR/scpi
	;;

  serve_pwd)
    cp serve_pwd.py $BIN_DIR/serve_pwd
  ;;

  ssh-rm-host)
    cp ssh-rm-host.sh $BIN_DIR/ssh-rm-host
    chmod 755 $BIN_DIR/ssh-rm-host
  ;;

  *)
    echo "$0: Unknown target to deploy"
    exit 1
    ;;
esac
