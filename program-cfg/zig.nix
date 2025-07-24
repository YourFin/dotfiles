{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.packages = with pkgs; [
    zig
    zls
  ];
}
