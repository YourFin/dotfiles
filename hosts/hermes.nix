{
  config,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    ./machine-types/linux-gui.nix
    ./localhost.nix
  ];
  home.packages = with pkgs; [
    jetbrains.idea-community
    android-studio
    guile_3_0
    ryujinx
    # platformio-core
    platformio
  ];
}
