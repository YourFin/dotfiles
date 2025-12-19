{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ../program-cfg/ghostty.nix
    ./extended-cli.nix
  ];
  home.packages = with pkgs; [
    (lib.hiPrio gcc)
    getent
    coreutils
    yt-dlp
    swiftdefaultapps
    rectangle
    nerd-fonts.fira-code
    nerd-fonts.fira-mono
    nerd-fonts.symbols-only
    yf.serious-sans
  ];
  services.syncthing.enable = true;
  fonts.fontconfig.enable = true;
  home.sessionPath = [
    "/sbin"
    "/bin"
    "/usr/bin"
  ];

  targets.darwin.currentHostDefaults."com.apple.controlcenter".BatteryShowPercentage = true;
}
