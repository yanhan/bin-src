#!/bin/bash

readonly bin_name=kind-with-registry
rm -f ./"${bin_name}"
go build
mv ./"${bin_name}" "${HOME}"/bin/
