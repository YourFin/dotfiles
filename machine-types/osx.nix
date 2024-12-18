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
    coreutils
    yt-dlp
    swiftdefaultapps
    rectangle
    (callPackage ../program-cfg/serious-sans { })
    (nerdfonts.override {
      fonts = [
        "FiraCode"
        "FiraMono"
        "NerdFontsSymbolsOnly"
      ];
    })
  ];
  services.syncthing.enable = true;
  services.emacs = {
    client.enable = true;
  };
  fonts.fontconfig.enable = true;

  targets.darwin.currentHostDefaults."com.apple.controlcenter".BatteryShowPercentage = true;
}
