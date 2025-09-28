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
    pgtk = lib.mkOption {
      type = with lib.types; bool;
      default = false;
      description = "Use pgtk package for emacs <i.e. wayland>";
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
    home.file.".config/yf/emacs/config.el".text = ''
      (setq treesit-extra-load-path '("${pkgs.yf.tree-sitter-bundle}/lib"))
    '';
    home.packages = with pkgs; [ typescript-language-server ];
    programs.emacs.enable = true;
    programs.emacs.package = pkgs.emacs-pgtk;
    programs.emacs.extraPackages = epkgs: [ epkgs.vterm ];
  };
}
