#!/bin/bash

set -euo pipefail
IFS=$'\n\t'

gcd() {
	local x=${1}
	local y=${2}
	local tmp
	while [ "${y}" -gt 0 ]; do
		tmp=${y}
		y=$(( x % y ))
		x=${tmp}
	done
	echo "${x}"
}

main() {
	local output_dir=transformed
	if [ ! -d "${output_dir}" ]; then
		mkdir -v "${output_dir}"
	fi
	local all_images
	all_images="$(ls ./*.jpg)"

	local img
	local image_size_line
	local height
	local width
	local divisor
	local aspect_ratio
	local width_ratio
	local height_ratio
	for img in ${all_images[@]}; do
		image_size_line="$(exiftool "${img}" | grep '^Image Size')"
		width="$(sed 's/^Image Size[[:space:]]\{1,\}:[[:space:]]*\([[:digit:]]\{1,\}\)x[[:digit:]]\{1,\}$/\1/' <<<"${image_size_line}")"
		height="$(sed 's/^Image Size[[:space:]]\{1,\}:[[:space:]]*[[:digit:]]\{1,\}x\([[:digit:]]\{1,\}\)$/\1/' <<<"${image_size_line}")"
		divisor="$(gcd ${width} ${height})"
		if [ "${divisor}" -gt 1 ]; then
			size_now="${width}x${height}"
			width_ratio=$(( width / divisor ))
			height_ratio=$(( height / divisor ))
			aspect_ratio="${width_ratio}x${height_ratio}"
			case "${aspect_ratio}" in
				16x9)
					if [ "${width}" -gt 1280 ]; then
						convert "${img}" -resize 1280x720 "${output_dir}/${img}"
					else
						printf "Copying %s of size %s because it is small enough.\n"  "${img}"  "${size_now}"
						cp "${img}"  "${output_dir}/${img}"
					fi
					;;
				4x3)
					if [ "${width}" -gt 1024 ]; then
						convert "${img}" -resize 1024x768 "${output_dir}/${img}"
					else
						printf "Copying %s of size %s because it is small enough.\n"  "${img}"  "${size_now}"
						cp "${img}"  "${output_dir}/${img}"
					fi
					;;
				*)
					printf "Need to support aspect ratio %s for image %s\n"  "${aspect_ratio}"  "${img}"
					;;
			esac
		else
			printf "Cannot find divisor > 1 for image %s\n"  "${img}"
		fi
	done
}

main "$@"
# vim:noet
