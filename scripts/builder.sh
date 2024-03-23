#!/bin/bash

set -euxo pipefail
shopt -s nullglob

for folder in "${src}"/*; do
    echo "$folder"
    foldername="$("$coreutils/bin/basename" "${folder}")"
    "$coreutils/bin/mkdir" -p "${out}/bin/"
    if [ -d "${folder}" ]; then
        if [ -f "${folder}/binaryName" ]; then
            binaryName="$(<"${folder}/binaryName")"
        else
            binaryName="${foldername}"
        fi
        for script in "${folder}"/*; do
            if [[ "${script}" == 'binaryName' ]]; then
                continue
            fi
            scriptname="$("$coreutils/bin/basename" "${script}")"
            scriptoutpath="${out}/bin/${scriptname}"
            if [ -e "$scriptoutpath" ]; then
                echo "Error: duplicate script named $scriptname"
                exit 1
            fi
            "$coreutils/bin/cat" - "${folder}/${scriptname}" >"$scriptoutpath" <<EOF
#!${!foldername}/${binaryName}

EOF
            "$coreutils/bin/chmod" a+x "$scriptoutpath"
        done
    fi
done
