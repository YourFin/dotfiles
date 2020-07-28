{ config, lib, pkgs, ... }:

{
  imports = [ ../program-cfg/kitty.nix ./extended-cli.nix ];
  home.packages = with pkgs; [
    plasma5.bluedevil
    chromium
    discord
    flameshot
    keepass
    nextcloud-client
    pdfarranger
    spotify
    vlc

    nerdfonts

    youtube-dl
  ];
  fonts.fontconfig.enable = true;

  programs.emacs.enable = true;

  programs.firefox.enable = true;
}
