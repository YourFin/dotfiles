{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ../machine-types/linux-gui.nix
    ../program-cfg/python.nix
    ../program-cfg/zig.nix
  ];
  home.packages = with pkgs; [
    jetbrains.idea-community
    android-studio
    guile_3_0
    ryubing
    # platformio-core
    platformio
    wl-clipboard
  ];
}
