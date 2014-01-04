# About

A collection of source code for helpful command line utilities.

Feel free to copy, modify or distribute any content in this repository
for any use you see fit.

# System Requirements

* Bash
* Ruby

# Deployment

The deploy script is `./deploy.sh`. You will have to specify a target to deploy.
Targets are inside the `deploy.sh` file.

To deploy everything, simply run

    ./deploy.sh all

The default deployment directory is '$HOME/bin'. Feel free to modify the
`deploy.sh` script to change it.
