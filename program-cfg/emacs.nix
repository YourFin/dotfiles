{
  config,
  lib,
  pkgs,
  ...
}:

{
  options.yf.emacs = {
    features = lib.mkOption {
      type = with lib.types; listOf str;
      default = [ ];
      example = [ "python" ];
      description = "Corresponding (provide 'yf-feat-\$feature) statements to load in emacs";
    };
  };
  config = {
    home.activation.linkDoomConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      $DRY_RUN_CMD ln -sf $VERBOSE_ARG ${builtins.toPath ../program-cfg/doom} ${config.xdg.configHome}
    '';
    home.sessionPath = [ "${config.xdg.configHome}/emacs/bin" ];
    home.file.".config/yf/emacs/features.el".text = lib.concatMapStringsSep "\n" (
      feat: "(provide 'yf-feat-${feat})"
    ) config.yf.emacs.features;
    programs.emacs.enable = true;
    programs.emacs.package = pkgs.emacs30;
    programs.emacs.extraPackages = epkgs: [ epkgs.vterm ];
  };
}
