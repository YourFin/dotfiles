#!/bin/bash

set -euxo pipefail
shopt -s nullglob

for folder in "${src}"/*
do
    echo "$folder"
    foldername="$("$coreutils/bin/basename" "${folder}")"
    "$coreutils/bin/mkdir" -p "${out}/bin/"
    if [ -d "${folder}" ]; then
        for script in "${folder}"/*
        do
            scriptname="$("$coreutils/bin/basename" "${script}")"
            scriptoutpath="${out}/bin/${scriptname}"
            if [ -e "$scriptoutpath" ] ; then
                echo "Error: duplicate script named $scriptname"
                exit 1
            fi
            "$coreutils/bin/cat" - "${folder}/${scriptname}" >"$scriptoutpath" <<EOF
#!${!foldername}

EOF
            "$coreutils/bin/chmod" a+x "$scriptoutpath"
        done
    fi
done
