{ config, lib, pkgs, ... }:

{
  imports = [ ../program-cfg/kitty.nix ./extended-cli.nix ];
  home.packages = with pkgs; [
    chromium
    bitwarden
    discord
    flameshot
    keepass
    nextcloud-client
    pdfarranger
    spotify
    signal-desktop
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
  services.lorri.enable = true;

  programs.firefox.enable = true;
}
