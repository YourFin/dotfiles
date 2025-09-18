{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.packages = with pkgs; [
    waybar
    xwayland-satellite
    swaybg
    brightnessctl
    pwvucontrol
  ];
  home.activation.linkNiriConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
    $DRY_RUN_CMD mkdir -p $VERBOSE_ARG ${config.xdg.configHome}/niri
    $DRY_RUN_CMD ln -sf $VERBOSE_ARG ${builtins.toPath ./.}/niri/config.kdl ${config.xdg.configHome}/niri/config.kdl
  '';
  programs.fuzzel.enable = true;
  programs.swaylock.enable = true;
  services.mako.enable = true;
  services.swayidle.enable = true;
  services.polkit-gnome.enable = true;
  yf.emacs.pgtk = true;
  #home.sessionPath = [ "${GOPATH}/bin" ];
  #home.sessionVariables = {
  #  inherit GOPATH;
  #};
}
