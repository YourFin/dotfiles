{
  config,
  pkgs,
  lib,
  ...
}:
let
  isMac = (lib.systems.elaborate builtins.currentSystem).isDarwin;
  baseNix = if isMac then ./machine-types/osx.nix else ./machine-types/base.nix;
  imports =
    if builtins.pathExists ./localhost.nix then
      [
        ./localhost.nix
      ]
    else
      [ baseNix ];
  addPkgFrom = path: (self: super: { yf = super.callPackage path { } // super.yf; });
in
{
  nixpkgs.overlays = [
    (self: super: { yf = super.callPackage ./pkgs/nushell-builder.nix { }; })
    # Note: by creating one overlay per package, later package definitions
    # can depend on earlier ones
    (addPkgFrom ./pkgs/huggingface-git.nix)
    (addPkgFrom ./pkgs/tree-sitter-bundle.nix)
    (addPkgFrom ./pkgs/path-extractor.nix)
    (addPkgFrom ./pkgs/scripts)
    (addPkgFrom ./pkgs/serious-sans.nix)
    (addPkgFrom ./pkgs/yfnutool.nix)
  ];
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  ##############################################################################
  # Make any changes by editing ./local.nix or commiting changes in other files
  ##############################################################################

  # pull in these and export in current context
  imports = imports;

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.03";
}
