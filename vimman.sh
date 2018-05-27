#!/bin/bash

set -euo pipefail
IFS=$'\n\t'

main() {
	MANPAGER="/bin/sh -c \"col -b | vim --not-a-term -c 'set ft=man ts=8 nomod nolist noma' -\""  man "$@"
}

main "$@"
# vim:noet
