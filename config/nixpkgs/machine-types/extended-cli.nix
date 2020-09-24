{ config, lib, pkgs, ... }:

{
  imports = [ ./base.nix ];

  home.packages = with pkgs; [
    awscli
    nix-index
    nmap
    lnav

    clang
    nodejs
    ruby

    aspell
    aspellDicts.en
    aspellDicts.en-computers

    # Haskell
    cabal2nix
    ormolu

    python3

    # Emacs deps:
    shfmt
    nixfmt
    cmake # For vterm
    # libvterm
  ];

  programs.emacs.enable = true;

  programs.broot = {
    enable = true;
    enableZshIntegration = true;
    enableBashIntegration = false;
    enableFishIntegration = false;
    verbs = {
      "line_downmeow" = {
        key = "ctrl-j";
        execution = ":line_down";
      };
      "line_upmeow" = {
        key = "ctrl-k";
        execution = ":line_up";
      };
      "quitmeow" = {
        key = "ctrl-g";
        execution = ":quit";
      };
      "vim" = {
        invocation = "vim";
        execution = "vim {file}";
      };
    };
  };
}
