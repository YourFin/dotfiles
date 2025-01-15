{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ../program-cfg/kitty.nix
    ./extended-cli.nix
  ];
  home.packages = with pkgs; [
    (hiPrio gcc)
    coreutils
    yt-dlp
    swiftdefaultapps
    rectangle
    nerd-fonts.fira-code
    nerd-fonts.fira-mono
    nerd-fonts.symbols-only
    (callPackage ../program-cfg/serious-sans { })
  ];
  services.syncthing.enable = true;
  services.emacs = {
    client.enable = true;
  };
  fonts.fontconfig.enable = true;

  targets.darwin.currentHostDefaults."com.apple.controlcenter".BatteryShowPercentage = true;
}
