{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ./base.nix
    ../program-cfg/npm.nix
  ];

  home.packages =
    with pkgs;
    [
      awscli
      dhall
      dhall-json
      github-cli
      graphviz
      jq
      lnav
      nix-prefetch-scripts
      nmap
      pandoc
      unzip
      syncthing
      zip

      parallel

      clang_17
      clang-tools_17
      gnumake
      zig

      nodejs
      python3
      ruby
      swiProlog

      (aspellWithDicts (dicts: [
        dicts.en
        dicts.en-computers
      ]))

      # Haskell
      cabal2nix
      ormolu
      stack

      rustc
      cargo
      clippy

      # Scala lang server
      metals

      # Emacs deps:
      shfmt
      nixfmt-rfc-style
      poppler
      cmake # For vterm
      shellcheck
      editorconfig-core-c
      # libvterm
    ]
    ++ (
      if (lib.systems.elaborate builtins.currentSystem).isDarwin then
        [ ]
      else
        [
          strace
          nethogs
          # diffoscope
          wcc # Witchcraft compiler collection
        ]
    );

  home.file.".stack/config.yaml".source = ../share/stackConfig.yaml;

  programs.emacs.enable = true;
  programs.emacs.package = pkgs.emacs29;
  programs.emacs.extraPackages = epkgs: [ epkgs.vterm ];
  home.activation.linkDoomConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD ln -sf $VERBOSE_ARG ${builtins.toPath ../program-cfg/doom} ${config.xdg.configHome}
  '';

  programs.man.generateCaches = true;
  programs.nix-index.enable = true;
}
