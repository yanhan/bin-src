#!/bin/bash

set -euo pipefail
IFS=$'\n\t'

# About: views manpage in vim
# Source: https://twitter.com/nixcraft/status/973221066012229633?s=03

main() {
	MANPAGER="/bin/sh -c \"col -b | vim --not-a-term -c 'set ft=man ts=8 nomod nolist noma' -\""  man "$@"
}

main "$@"
# vim:noet
