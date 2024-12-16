{
  config,
  lib,
  pkgs,
  ...
}:

let
  GOPATH = "${config.home.homeDirectory}/g/go";
in

{
  home.packages = with pkgs; [
    go
  ];
  home.sessionPath = [ "${GOPATH}/bin" ];
  home.sessionVariables = {
    inherit GOPATH;
  };
}
