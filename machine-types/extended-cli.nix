{ config, lib, pkgs, ... }:

{
  imports = [ ./base.nix ];

  home.packages = with pkgs;
    [
      awscli
      nix-index
      nmap
      lnav
      jq
      dhall
      dhall-json

      clang
      nodejs
      ruby

      aspell
      aspellDicts.en
      aspellDicts.en-computers

      # Haskell
      cabal2nix
      ormolu
      stack

      python3

      # Emacs deps:
      shfmt
      nixfmt
      poppler
      cmake # For vterm
      # libvterm
    ]
    ++ (if builtins.currentSystem == "x86_64-darwin" then [ ] else [ strace ]);

  home.file.".npmrc".source = ../share/dot/npmrc;
  home.file.".stack/config.yaml".source = ../share/stackConfig.yaml;

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
