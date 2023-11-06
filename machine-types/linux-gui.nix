{ config, lib, pkgs, ... }:

{
  imports = [ ../program-cfg/kitty.nix ./extended-cli.nix ];
  home.packages = with pkgs; [
    chromium
    bitwarden
    bitwarden-cli
    discord
    flameshot
    gimp
    gnome-multi-writer
    keepass
    pdfarranger
    spotify
    signal-desktop
    vlc
    (callPackage ../program-cfg/serious-sans { })

    (nerdfonts.override {
      fonts = [
        "FiraCode"
        "FiraMono"
        "NerdFontsSymbolsOnly"
        "AnonymousPro"
        "ShareTechMono"
        "ProFont"
        "Monofur"
        "Inconsolata"
        "IntelOneMono"
        "Gohu"
        "3270"
      ];
    })

    youtube-dl
  ];
  fonts.fontconfig.enable = true;

  services.emacs = {
    enable = true;
    client.enable = true;
  };
  services.syncthing.enable = true;
  services.lorri.enable = true;

  programs.firefox.enable = true;
}
