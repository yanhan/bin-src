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
CABAL_VERSION_WANT=1.18.0.2
CABAL_GIT_TAG_CHECKOUT=Cabal-v1.18.1.2

package_installed() {
  dpkg -s $1 >/dev/null 2>&1
}

install_package() {
  echo "$1 not installed, calling $APT_GET to install it"
  sudo APT_GET install $1
}

exit_fatal () {
  echo "Fatal: $1. Exiting."
  exit 1
}

if [ `cabal --numeric-version` = $CABAL_VERSION_WANT ]
then
  echo "cabal is up to date (version $CABAL_VERSION_WANT). Exiting."
  exit 0
fi

if ! which $APT_GET >/dev/null
then
  echo "Fatal: Sorry, only systems with $APT_GET are supported"
  exit 1
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

echo "Updating system..."
sudo $APT_GET update

if ! package_installed haskell-platform
then
  install_package haskell-platform
fi

if ! package_installed cabal-install
then
  install_package cabal-install
fi

if ! package_installed git
then
  install_package git
fi

cd $HOME

if ! [[ -d cabal ]]
then
  git clone 'https://github.com/haskell/cabal.git' cabal
fi

# Assume we are not actually cabal hackers
cd cabal
git checkout $CABAL_GIT_TAG_CHECKOUT
cabal install Cabal/ cabal-install/

# final messages
echo "Done."
echo 'Please append $HOME/.cabal/bin to your $PATH'
