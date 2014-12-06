# About

A collection of source code for helpful command line utilities.

Feel free to copy, modify or distribute any content in this repository
for any use you see fit.

# System Requirements

* Bash
* Ruby
* Haskell Platform

## Setup

NOTE: This should be done before you build / deploy anything.
Please ensure that you fulfil the system requirements before this step.
Otherwise, please install them manually.

Run the `setup_cabal.sh` script. It will try to install the version of
Cabal in the `CABAL_VERSION_WANT` variable in `setup_cabal.sh`.
This is required for Cabal sandbox.

Run the `setup.sh` script. It will setup a Cabal Sandbox for the Haskell
programs in this repository and install the necessary Haskell dependencies.

### Setup virtualenv:

    virtualenv venv

### Install requirements

    . venv/bin/activate
    pip install -r requirements.txt

### Setup sbt

    . venv/bin/activate
    ./setup.py

# Deployment

The deploy script is `./deploy.sh`. You will have to specify a target to deploy.
Targets are inside the `deploy.sh` file.

To deploy everything, simply run

    ./deploy.sh all

The default deployment directory is '$HOME/bin'. Feel free to modify the
`deploy.sh` script to change it.

# Building

The build script is `build.sh`. You must specify a build target. Build targets
are available inside the `build.sh` file.
