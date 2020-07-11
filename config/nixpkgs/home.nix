{ config, pkgs, ... }:

let 
  unstable = import <unstable> {};
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # pull in these and export in current context
  imports = [ ./kitty.nix ];

  fonts.fontconfig.enable = true;

  home.packages = with pkgs; [
    # Graphical applications
    plasma5.bluedevil
    chromium
    discord
    flameshot
    keepass
    nextcloud-client
    pdfarranger
    spotify
    vlc

    # Fonts
    unstable.nerdfonts

    # Command line utils
    awscli
    bat
    exa
    fd
    fzf
    ncdu
    git
    ripgrep
    ripgrep-all

    ruby
    python

    # Less-standard cli tools
    clang
    youtube-dl

    # Networking
    nmap
   
    # TUI
    htop
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    
    # Haskell
    cabal2nix
    ormolu

    # Emacs deps:
    shfmt
    nixfmt
  ];

  programs.emacs.enable = true;

  programs.firefox.enable = true;

  programs.direnv = {
    enable = true;
    #enableNixDirenvIntegration = true;
  };

  programs.broot = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = false;
    enableFishIntegration = false;
    verbs = {
      "line_downmeow" = { key = "ctrl-j"; execution = ":line_down"; };
      "line_upmeow" = { key = "ctrl-k"; execution = ":line_up"; };
      "quitmeow" = { key = "ctrl-g"; execution = ":quit"; };
      "vim" = { invocation = "vim"; execution = "vim {file}"; };
    };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.03";
}
