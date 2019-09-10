#!/bin/bash

IFS=$'\n\t'
set -euo pipefail

main() {
	local source_extension=${1}
	local dest_extension=${2}
	local f
	for f in *."${source_extension}"; do
		mv "${f}"  "${f%.${source_extension}}.${dest_extension}"
	done
}

main "$@"
# vim:noet
