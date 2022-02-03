{ config, lib, pkgs, ... }:

{
  home.file.".npmrc".text = ''
    prefix=${config.home.homeDirectory}/.local/share/npm/global/
    store=${config.home.homeDirectory}/.local/share/npm/npm-store
    shell=zsh
  '';
}
