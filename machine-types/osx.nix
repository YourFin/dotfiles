{ config, lib, pkgs, ... }:

{
  imports = [ ../program-cfg/kitty.nix ./extended-cli.nix ];
  home.packages = with pkgs; [
    coreutils
    youtube-dl
    swiftdefaultapps
    rectangle
    (nerdfonts.override { fonts = [ "FiraCode" "FiraMono" ]; })
    (callPackage ../program-cfg/serious-sans { })
  ];
  services.syncthing.enable = true;
  services.emacs = { client.enable = true; };
  fonts.fontconfig.enable = true;

  targets.darwin.currentHostDefaults."com.apple.controlcenter".BatteryShowPercentage =
    true;
}
