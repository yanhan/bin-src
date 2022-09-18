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
      cp ssh-rm-host.sh $BIN_DIR/ssh-rm-host
      cd $BIN_DIR
      chmod 700 ssh-rm-host
    else
      echo "Deploy failed"
    fi
    ;;

  kind-with-registry|kwr)
    if [ ! -d "${BIN_DIR}"/kind-templates ]; then
      cp -R kind-templates "${BIN_DIR}"/
    fi
    cp kind-with-registry.sh "${BIN_DIR}"/
    cp kind-with-registry.sh "${BIN_DIR}"/kwr
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
