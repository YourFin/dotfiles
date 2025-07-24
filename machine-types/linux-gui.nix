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
    discord
    ghostty
    proton-pass
    flameshot
    showmethekey
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

  home.shellAliases = {
    firefox = "firefox --new-window";
  };

  services.emacs = {
    enable = true;
    client.enable = true;
  };
  services.syncthing.enable = true;
  services.lorri.enable = true;

  programs.firefox.enable = true;
  gtk = {
    enable = true;
    theme = {
      name = "Orchis-Dark";
      package = pkgs.orchis-theme;
    };
  };
  programs.gnome-shell.theme = {
    name = "Orchis-Dark";
    package = pkgs.orchis-theme;
  };
  home.pointerCursor = {
    enable = true;
    gtk.enable = true;
    package = pkgs.bibata-cursors;
    name = "Bibata-Original-Ice";
    size = 24;
  };
  dconf = {
    enable = true;
    settings."org/gnome/shell" = {
      disable-user-extensions = false;
      enabled-extensions =
        with pkgs.gnomeExtensions;
        map (ext: ext.extensionUuid) [
          wiggle
          gsconnect
          burn-my-windows
          user-themes
        ];
    };
  };
}
