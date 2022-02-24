#!/usr/bin/env bash

# Based on https://kind.sigs.k8s.io/docs/user/local-registry/
#
# ABOUT
# =====
# Creates a kind cluster with a local docker registry.
# The cluster has 1 control plane node and 3 worker nodes.
# If you encounter issues creating more than 1 cluster, please adjust the
# sysctl flags, as detailed in
#
#    https://github.com/kubernetes-sigs/kind/issues/2491#issuecomment-943067643
#
# NOTE: This is for usage on a dev machine only. There are no security
# controls for the cluster or the docker registry

set -euo pipefail
IFS=$'\n\t'

declare -r REGISTRY_CONTAINER_NAME=kind-registry
declare -r CONTAINER_PORT=5000
declare -r DEFAULT_HOST_PORT=6001
declare -r TEMPLATES_DIR="$(dirname "$(realpath "${BASH_SOURCE[0]}")")"/kind-templates

# Create docker container for the local registry, based on the image
# https://hub.docker.com/_/registry
# This single container is reusable across different kind clusters
create_registry_container() {
	declare -r reg_name="${REGISTRY_CONTAINER_NAME}"
	declare -r reg_port=${1}
	if [ "$(docker inspect -f '{{.State.Running}}'  "${reg_name}" 2>/dev/null || true)" != 'true' ]; then
		printf "Creating docker registry container which will listen at 127.0.0.1:%d\n"  "${reg_port}"
		docker run \
			-d --restart=always \
			-p 127.0.0.1:${reg_port}:${CONTAINER_PORT} \
			--name "${reg_name}" \
			registry:2
	fi
}

create_kind_cluster() {
	declare -r tempdir=${1}
	declare -r cluster_name=${2}
	declare -r port=${3}
	local rc

	set +e
	kind get clusters | grep -q '^'"${cluster_name}"'$'
	rc=${?}
	set -e

	if [ "${rc}" -ne 0 ]; then
		local kind_config_path="${tempdir}"/three-nodes-with-registry.yml
		sed -e "s/\${host_port}/${port}/" \
			-e "s/\${container_port}/${CONTAINER_PORT}/" \
			"${TEMPLATES_DIR}"/three-nodes-with-registry.yml.tmpl >"${kind_config_path}"
		kind create cluster --name "${cluster_name}" --config "${kind_config_path}"
	fi
}

connect_registry_to_cluster_network() {
	if [ "$(docker inspect -f='{{json .NetworkSettings.Networks.kind}}'  "${REGISTRY_CONTAINER_NAME}")" = "null" ]; then
		docker network connect kind  "${REGISTRY_CONTAINER_NAME}"
	fi
}

add_configmap_for_local_registry() {
	declare -r tempdir=${1}
	declare -r port=${2}

	local cm_path="${tempdir}"/cm.yml
	sed -e "s/\${port}/${port}/"  "${TEMPLATES_DIR}"/cm.yml.tmpl >"${cm_path}"
	kubectl apply -f "${cm_path}"
}

usage() {
	printf "Creates a kind cluster with a local docker registry.\n"
	printf "\nUsage:\n"
	printf "\n"
	printf "\t%s -n CLUSTER_NAME [-p REGISTRY_PORT]\n"  "${1}"
	printf "\nOptions:\n"
	printf "\n"
	printf "\t-n: name of the kind cluster to create\n"
	printf "\t-p: port of the local docker registry to create. Defaults to %s if not supplied\n"  "${DEFAULT_HOST_PORT}"
	exit 1
}

main() {
	declare -r optstring=':hn:p:'
	declare -r program_name=${0}
	local opt
	local cluster_name=""
	local has_error=false
	local port=""
	local rc
	local tempdir
	while getopts "${optstring}" opt "${@}"; do
		case "${opt}" in
			h)
				usage "${program_name}"
				;;
			\:)
				printf >&2 "Error: missing argument to -%s\n"  "${OPTARG}"
				has_error=true
				;;
			\?)
				printf >&2 "Error: unsupported flag -%s\n"  "${OPTARG}"
				has_error=true
				;;
			n)
				cluster_name=${OPTARG}
				set +e
				grep -q '^[a-za-Z][a-zA-Z0-9\.-]*$' <<< "${cluster_name}"
				rc=${?}
				set -e
				if [ "${rc}" -ne 0 ]; then
					printf >&2 "Error: cluster name must start with alphabet, followed by alphanumeric / period / dash characters\n"
					has_error=true
				fi
				;;
			p)
				port=${OPTARG}
				set +e
				grep -q '^[[:digit:]]\{1,\}$' <<< "${port}"
				rc=${?}
				set +e
				if [ "${rc}" -ne 0 ] || [ "${port}" -lt 1 ] || [ "${port}" -gt 65535 ]; then
					printf >&2 "Error: port must be a number from 1 to 65535\n"
					has_error=true
				fi
				set -e
				;;
		esac
	done

	if [ "x${cluster_name}" = "x" ]; then
		printf >&2 "Error: cluster name must be supplied via -n\n"
		has_error=true
	fi

	if [ "x${port}" = "x" ]; then
		port=${DEFAULT_HOST_PORT}
	fi

	if [ "${has_error}" = "true" ]; then
		printf >&2 "Exiting\n"
		exit 1
	fi

	tempdir="$(mktemp -d XXXXXXX)"
	create_registry_container "${port}"
	create_kind_cluster "${tempdir}"  "${cluster_name}"  "${port}"
	connect_registry_to_cluster_network
	add_configmap_for_local_registry  "${tempdir}"  "${port}"

	rm -rf "${tempdir}"
}

main "$@"
