{ config, lib, pkgs, ... }:

{
  imports = [ ../program-cfg/kitty.nix ./extended-cli.nix ];
  home.packages = with pkgs; [
    coreutils
    youtube-dl

    nerdfonts
  ];
  fonts.fontconfig.enable = true;
  programs.emacs.enable = true;
}
