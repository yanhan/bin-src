#!/bin/bash

set -euo pipefail
IFS=$'\n\t'

main() {
	local return_code

	set +e
	git rev-parse --show-toplevel >/dev/null 2>&1
	return_code="${?}"
	set -e

	if [ "${return_code}" -ne 0 ]; then
		printf >&2 "You are not in a git repository. Exiting.\n"
		exit 1
	fi

	if [ "${#}" -ne 2 ]; then
		printf >&2 "Usage: %s  <new author name>  <new author email>\n"  "${0}"
		printf >&2 "Exiting.\n"
		exit 1
	fi

	local new_author_name=${1}
	local new_author_email=${2}
	# Modify git commit author and committer for all commits
	git filter-branch -f --env-filter "GIT_AUTHOR_NAME='${new_author_name}'; GIT_AUTHOR_EMAIL='${new_author_email}'; GIT_COMMITTER_NAME='${new_author_name}'; GIT_COMMITTER_EMAIL='${new_author_email}';" HEAD
	# Remove all Signed-off-by lines
	git filter-branch -f --msg-filter "sed 's/Signed-off-by: .*//g'" HEAD
	# Add new Signed-off-by line
	git filter-branch -f --msg-filter "cat - && echo && git interpret-trailers --trailer 'Signed-off-by: ${new_author_name} <${new_author_email}>'" HEAD
	# Remove consecutive blank lines
	git filter-branch -f --msg-filter "sed 'N;/^\\n$/D;P;D;'" HEAD
}

main "$@"
# vim:noet
