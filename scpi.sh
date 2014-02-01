#!/bin/bash

# A shortcut for the `scp` program that makes use of the user's SSH config file
# so that we do not have to type out the full server name

# Usage:
#
# - Copying file from remote host to local machine:
#
#     scpi hostname:~/somedir/somefile myCopiedFile

MY_SSH_CONFIG_FILE=~/.ssh/config

scp -i $MY_SSH_CONFIG_FILE $@
