{ config, lib, pkgs, ... }:

{
  home.file.".npmrc".text = ''
    prefix=${config.home.homeDirectory}/.local/usr/npm/global/
    store=${config.home.homeDirectory}/.local/usr/npm/npm-store
    shell=zsh
  '';
}
