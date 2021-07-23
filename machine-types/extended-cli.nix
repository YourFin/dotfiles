{ config, lib, pkgs, ... }:

{
  imports = [ ./base.nix ../program-cfg/npm.nix ];

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

      rustc
      cargo
      clippy
      nix-prefetch-scripts

      python3

      # Emacs deps:
      shfmt
      nixfmt
      poppler
      cmake # For vterm
      # libvterm
    ]
    ++ (if builtins.currentSystem == "x86_64-darwin" then [ ] else [ strace ]);

  home.file.".stack/config.yaml".source = ../share/stackConfig.yaml;

  programs.emacs.enable = true;
}
