#!/usr/bin/env bash

set -euo pipefail

SOURCE=${BASH_SOURCE[0]}
while [ -L "$SOURCE" ]; do # resolve $SOURCE until the file is no longer a symlink
  DIR=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )
  SOURCE=$(readlink "$SOURCE")
  [[ $SOURCE != /* ]] && SOURCE=$DIR/$SOURCE # if $SOURCE was a relative symlink, we need to resolve it relative to the path where the symlink file was located
done
script_dir=$( cd -P "$( dirname "$SOURCE" )" >/dev/null 2>&1 && pwd )

# Generated mostly by chat gpt

# Create the "hosts" folder if it doesn't already exist
mkdir -p "$script_dir/hosts"

# Determine the current system (either "macOS" or "Linux")
if [[ "$OSTYPE" == "darwin"* ]]; then
  # Get the host CPU architecture
  arch=$(uname -m)
  if [[ "$arch" == "arm64" ]]; then
    # M1 Mac
    system="aarch64-darwin"
  else
    # Intel-based Mac
    system="x86_64-darwin"
  fi
else
  # Linux-based system
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
cat << EOF > "$script_dir/hosts/$(hostname).nix"
{
  home-dir = "$HOME";
  username = "$(whoami)";
  hostname = "$(hostname)";
  system = "$system";
}
EOF
ln -s "./hosts/$(hostname).nix" "$script_dir/localhost.nix"
