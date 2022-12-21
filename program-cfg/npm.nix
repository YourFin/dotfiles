{ config, lib, pkgs, ... }:

let
  dirs = rec {
    npm = "${config.xdg.cacheHome}/npm";
    prefix = "${npm}/global/";
    logs = "${npm}/logs/";
    store = "${npm}/store/";
  };
in {
  home.activation.initNpmFolders = lib.hm.dag.entryAfter [ "writeBoundary" ] (''
    # Purposely not differentiating between unset arg
    # and empty, as they're the same here
    if ! [ -z "$VERBOSE_ARG" ]; then
      VERBOSE_ARG="-v"
    fi

  '' + (pkgs.lib.concatMapStringsSep "\n" (folder: ''
    $DRY_RUN_CMD mkdir -p $VERBOSE_ARG ${folder}
  '') (pkgs.lib.attrValues dirs)));

  home.file.".npmrc".text = ''
    prefix=${dirs.prefix}
    store=${dirs.store}
    logs-dir=${dirs.logs}
    shell=zsh
  '';
}
