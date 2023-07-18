{ config, lib, pkgs, ... }:

{
  imports = [ ../program-cfg/kitty.nix ./extended-cli.nix ];
  home.packages = with pkgs; [
    coreutils
    youtube-dl
    swiftdefaultapps
    rectangle

    (nerdfonts.override { fonts = [ "FiraCode" "FiraMono" ]; })
  ];
  services.emacs = { client.enable = true; };
  fonts.fontconfig.enable = true;
}
