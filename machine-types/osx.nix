{ config, lib, pkgs, ... }:

{
  imports = [ ../program-cfg/kitty.nix ./extended-cli.nix ];
  home.packages = with pkgs; [
    coreutils
    youtube-dl

    (nerdfonts.override { fonts = [ "FiraCode" "FiraMono" ]; })
  ];
  fonts.fontconfig.enable = true;
}
