#!/usr/bin/env bash

set -euo pipefail

script_dir=$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")
cd "${script_dir}"

# Generated mostly by chat gpt

# Create the "hosts" folder if it doesn't already exist
mkdir -p "$script_dir/hosts"

# Determine the current system (either "macOS" or "Linux")
if [[ "$OSTYPE" == "darwin"* ]]; then
  base_file="../machine-types/osx.nix"
  # Get the host CPU architecture
  arch=$(uname -m)
  if [[ "$arch" == "arm64" ]]; then
    # arm Mac
    system="aarch64-darwin"
  else
    # Intel-based Mac
    system="x86_64-darwin"
  fi
else
  # Linux-based system
  base_file="../machine-types/base.nix"
  arch=$(uname -m)
  if [[ "$arch" == "arm64" ]]; then
    # ARM-based Linux
    system="aarch64-linux"
  else
    # x86-based Linux
    system="x86_64-linux"
  fi
fi

# Write the home directory and username to a file in the "hosts" folder, using the current hostname as the file name
cat <<EOF >"$script_dir/hosts/$(hostname).nix"

{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [ ${base_file} ];
  home.packages = with pkgs; [
    # machine-specific-nonsense
  ];
}
EOF

# Set up default symlink
ln -s "./hosts/$(hostname).nix" "$script_dir/localhost.nix"
