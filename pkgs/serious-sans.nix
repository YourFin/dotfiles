{
  fetchgit,
  fetchzip,
  writeShellApplication,
  runCommand,
  fontforge,
  python3,
  parallel,
}:

let
  seriousSansRepo = fetchgit {
    url = "https://github.com/kaBeech/serious-sans.git";
    sparseCheckout = [ "SeriousSans/otf" ];
    rev = "b5eac940ed198f03270fff3313ff1096decdaa4b";
    hash = "sha256-0e03ScHNH+Hdhs0dOvmqcDddUmgTZypLuuhIPYDIAY4=";
  };

  nerdFontPatcherRelease = fetchzip {
    url = "https://github.com/ryanoasis/nerd-fonts/releases/download/v3.0.2/FontPatcher.zip";
    stripRoot = false;
    hash = "sha256-ZJpF/Q5lfcW3srb2NbJk+/QEuwaFjdzboa+rl9L7GGE=";
  };

  nerdFontPatchCurrentDir = writeShellApplication {
    name = "nerdfont-patch-current-dir";
    runtimeInputs = [
      fontforge
      (python3.withPackages (ps: [
        ps.fontforge
      ]))
      parallel
    ];
    text = ''
      find ./*.otf | parallel -j 8 python ${nerdFontPatcherRelease}/font-patcher '{}'
    '';
  };
in
{
  serious-sans = runCommand "serious-sans nerd font" { } ''
    mkdir -p $out/share/fonts/opentype/NerdFonts/
    cp ${seriousSansRepo}/SeriousSans/otf/* $out/share/fonts/opentype/NerdFonts/
    cd $out/share/fonts/opentype/NerdFonts/
    rm *Nerd.otf
    ${nerdFontPatchCurrentDir}/bin/nerdfont-patch-current-dir
    # find . ! -name "*Nerd*" -delete
  '';
}
