{ config, lib, pkgs, ... }:

{
  imports = [ ../program-cfg/kitty.nix ./extended-cli.nix ];
  home.packages = with pkgs; [
    chromium
    discord
    flameshot
    keepass
    nextcloud-client
    pdfarranger
    spotify
    vlc

    (nerdfonts.override { fonts = [ "FiraCode" "FiraMono" ]; })

    youtube-dl
  ];
  fonts.fontconfig.enable = true;

  services.emacs = {
    enable = true;
    client.enable = true;
  };
  services.nextcloud-client.enable = true;

  programs.firefox.enable = true;
}
