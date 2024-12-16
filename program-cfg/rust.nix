{
  config,
  lib,
  pkgs,
  ...
}:

let
  CARGO_HOME = "${config.home.homeDirectory}/.local/cargo";
in

{
  home.packages = with pkgs; [
    rustc
    cargo
    clippy
  ];
  home.sessionPath = [ "${CARGO_HOME}/bin" ];
  home.sessionVariables = {
    inherit CARGO_HOME;
  };
}
