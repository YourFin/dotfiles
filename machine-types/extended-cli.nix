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
    ../program-cfg/rust.nix
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
      pueue

      clang_17
      clang-tools_17
      gnumake
      zig

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
          strace
          plocate
          nethogs
          # diffoscope
          wcc # Witchcraft compiler collection
        ]
    );

  home.file.".stack/config.yaml".source = ../share/stackConfig.yaml;

  programs.emacs.enable = true;
  programs.emacs.package = pkgs.emacs29;
  programs.emacs.extraPackages = epkgs: [ epkgs.vterm ];
  home.shellAliases = {
    restart-emacs = "pushd ~ >/dev/null; while pkill emacs; do ; done && emacs --daemon ; popd >/dev/null";
    urldecode = ''
      python3 -c "import sys, urllib.parse as ul; \
          print(ul.unquote_plus(sys.argv[1]))"'';
    urlencode = ''
      "python3 -c "import sys, urllib.parse as ul; \
      		print(ul.quote_plus(sys.argv[1]))"'';
  };
  home.sessionPath = [ "${config.xdg.configHome}/emacs/bin" ];

  services.pueue = {
    enable = true;
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
  home.activation.linkDoomConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD ln -sf $VERBOSE_ARG ${builtins.toPath ../program-cfg/doom} ${config.xdg.configHome}
  '';

  programs.man.generateCaches = true;
  programs.nix-index.enable = true;
}
