{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ./base.nix
    ../program-cfg/emacs.nix
    ../program-cfg/npm.nix
    ../program-cfg/rust.nix
  ];

  home.packages =
    with pkgs;
    [
      awscli
      dhall
      dhall-json
      github-cli
      git-lfs
      graphviz
      jq
      lnav
      nix-prefetch-scripts
      nmap
      openssl
      pandoc
      syncthing
      unzip
      zip
      zstd

      parallel
      pueue

      clang_17
      clang-tools_17
      gnumake
      zig

      fastfetch # screenfetch replacement

      nodejs
      python3
      ruby
      swi-prolog

      (aspellWithDicts (dicts: [
        dicts.en
        dicts.en-computers
      ]))

      # Haskell
      cabal2nix
      ormolu
      stack

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
          sysz
          strace
          plocate
          nethogs
          # diffoscope
          wcc # Witchcraft compiler collection
        ]
    );

  programs.direnv = {
    enable = true;
    enableNushellIntegration = true;
    nix-direnv.enable = true;
  };

  home.file.".stack/config.yaml".source = ../share/stackConfig.yaml;

  home.shellAliases = {
  };

  services.pueue = {
    enable = (lib.systems.elaborate builtins.currentSystem).isLinux;
  };
  launchd.agents = (
    lib.mkMerge [
      (lib.mkIf pkgs.stdenv.isDarwin {
        pueue = {
          enable = true;
          config = {
            ProgramArguments = [
              "${pkgs.pueue}/bin/pueued"
              "-vv"
            ];
            RunAtLoad = true;
            KeepAlive = {
              Crashed = true;
              SuccessfulExit = false;
            };
          };
        };
      })
    ]
  );

  home.sessionVariables = {
    OPAMROOT = "$HOME/.local/usr/opam";
  };

  programs.man.generateCaches = true;
  programs.nix-index.enable = true;
}
