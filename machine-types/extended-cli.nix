{ config, lib, pkgs, ... }:

{
  imports = [ ./base.nix ../program-cfg/npm.nix ];

  home.packages = with pkgs;
    [
      awscli
      github-cli
      nix-index
      nmap
      lnav
      graphviz
      jq
      dhall
      dhall-json
      diffoscope
      unzip
      zip

      clang
      clang-tools
      nodejs
      ruby

      (aspellWithDicts (dicts: [ dicts.en dicts.en-computers ]))

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
      shellcheck
      editorconfig-core-c
      # libvterm
    ] ++ (if builtins.currentSystem == "x86_64-darwin" then
      [ ]
    else [
      strace
      wcc # Witchcraft compiler collection
    ]);

  home.file.".stack/config.yaml".source = ../share/stackConfig.yaml;

  programs.emacs.enable = true;
  programs.emacs.extraPackages = epkgs: [ epkgs.vterm ];
  home.activation.linkDoomConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD ln -s $VERBOSE_ARG ${
      builtins.toPath ../program-cfg/doom
    } ${config.xdg.configHome}
  '';
}
