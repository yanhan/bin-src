#!/bin/bash

# exit on undefined variables
set -u

# A bash script to install cabal (http://www.haskell.org/cabal/)
#
# Assumptions:
# 1. You are using a most probably Linux system with `apt-get` as
#    the package handling utility (sitting on top of `dpkg`)
#
# 2. $HOME is set, and you have read, write end execute permissions
#    for it
#
# 3. You are not a cabal developer. If you are, just modify the
#    script to clone the cabal repository to a different directory
#
# NOTE: This script makes use of `sudo`

APT_GET=apt-get
CABAL_VERSION_WANT=1.18.0.5
CABAL_GIT_TAG_CHECKOUT=Cabal-v1.18.1.2
SCRIPT_DIR=`pwd`

program_installed() {
  which -s $1
}

install_package() {
  echo "$1 not installed, calling $APT_GET to install it"
  sudo $APT_GET install $1
}

exit_fatal () {
  echo "Fatal: $1. Exiting."
  exit 1
}

if [ `cabal --numeric-version` = $CABAL_VERSION_WANT ]
then
  echo "cabal is up to date (version $CABAL_VERSION_WANT)."
  exit 0
fi

# check for presence of $HOME env var
if ! env | grep --quiet '^HOME='
then
  exit_fatal "Please set the 'HOME' environment variable"
fi

if ! [[ -d $HOME ]]
then
  exit_fatal '$HOME must be a directory'
fi

if ! [[ -r $HOME && -w $HOME && -x $HOME ]]
then
  exit_fatal "Fatal: Insufficient permissions for $HOME (needs read, write, execute)"
fi

if which $APT_GET >/dev/null
then
  echo "apt-get detected on your system."
  echo "Updating system..."
  sudo $APT_GET update

  if ! program_installed haskell-platform
  then
    install_package haskell-platform
  fi

  if ! program_installed cabal-install
  then
    install_package cabal-install
  fi

  if ! program_installed git
  then
    install_package git
  fi
else
  # system has no apt-get.
  # Check that GHC, cabal, git are installed
  if ! program_installed ghc
  then
    exit_fatal "ghc is missing. Please install it and try again"
  fi

  if ! program_installed cabal
  then
    exit_fatal "cabal is missing. Please install it and try again"
  fi

  if ! program_installed git
  then
    exit_fatal "git is missing. Please install it and try again"
  fi
fi

# Clone cabal and install it
CABAL_CLONE_DIR=`mktemp -d /tmp/setup_cabal_XXXXXX`
cd $CABAL_CLONE_DIR
echo "Cloning cabal to a temporary directory ($CABAL_CLONE_DIR)"

git clone 'https://github.com/haskell/cabal.git' cabal
cd cabal
git checkout $CABAL_GIT_TAG_CHECKOUT
cabal install Cabal/ cabal-install/
if [ $? -eq 0 ]
then
  # final messages
  echo "Done."
  echo "Please append/prepend the path to Cabal to your PATH environment variable"
else
  echo "Some Error occurred"
fi

cd $SCRIPT_DIR
rm -rf $CABAL_CLONE_DIR
