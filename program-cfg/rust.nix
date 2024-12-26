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
    rustfmt
    cargo
    clippy
    rust-analyzer
  ];
  home.sessionPath = [ "${CARGO_HOME}/bin" ];
  home.sessionVariables = {
    inherit CARGO_HOME;
  };
}
