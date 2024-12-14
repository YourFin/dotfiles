{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ../program-cfg/kitty.nix
    ./extended-cli.nix
  ];
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

    nerd-fonts.fira-code
    nerd-fonts.fira-mono
    nerd-fonts.symbols-only

    # nerd-fonts."AnonymousPro"
    # nerd-fonts."ShareTechMono"
    # nerd-fonts."ProFont"
    # nerd-fonts."Monofur"
    # nerd-fonts."Inconsolata"
    # nerd-fonts."IntelOneMono"
    # nerd-fonts."Gohu"
    # nerd-fonts."3270"

    yt-dlp
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
