# All other machine configurations inherit from this one
{
  config,
  lib,
  pkgs,
  ...
}:

{
  imports = [
    ../program-cfg/zsh.nix
    ../program-cfg/nushell.nix
    ../program-cfg/bash.nix
    ../program-cfg/vim.nix
  ];
  home.packages = with pkgs; [
    bat
    (lib.setPrio 100 binutils)
    eza
    fd
    git
    iftop
    mosh
    ncdu
    ripgrep
    gum
    inetutils
    vim
    screen
    zsh
    (callPackage ../scripts { })
    (callPackage ../program-cfg/path-extractor { })

    htop
    btop
  ];

  home.preferXdgDirectories = true;
  home.sessionVariables = {
    LANG = "en_US.UTF-8";
    EDITOR = "vim";
  };

  home.file.".config" = {
    source = ../share/dot/config;
    recursive = true;
  };

  home.file.".inputrc".source = ../share/dot/inputrc;
  home.file.".gitconfig".source = ../share/dot/gitconfig;

  programs.direnv = {
    enable = true;
    #enableNixDirenvIntegration = true;
    enableNushellIntegration = true;
  };

  programs.zoxide = {
    enable = true;
    enableBashIntegration = false;
    options = [ "--cmd j" ];
  };

  programs.mcfly = {
    enable = true;
    fuzzySearchFactor = 4;
    keyScheme = "vim";
  };
  programs.fzf = {
    enable = true;
    colors = {
      #      "bg+" = "#1b1720"; # from doom emacs
      "bg+" = "#27212e"; # Rasin black
      fg = "#91889b"; # Old Lavender
      "fg+" = "#ffffff"; # White
      "hl+" = "#eb64b9"; # Hot Pink
      prompt = "#b4dce7"; # Powder Blue
      spinner = "#b381c5"; # African Violet
      header = "#ffe261"; # Mustard
      marker = "#7b6995"; # Roman Silver
      pointer = "#eb64b9"; # Hot Pink
      info = "#74dfc4"; # Pearl Aqua
    };
  };

  home.activation."remove temporary .config/nixpkgs/config.nix file" =
    let
      fp = "${config.xdg.configHome}/nixpkgs/config.nix";
    in
    lib.hm.dag.entryBefore [ "checkLinkTargets" ] ''
      [[ -f "${fp}" ]] && ! [[ -L "${fp}" ]] &&
        $DRY_RUN_CMD rm $VERBOSE_ARG ${config.xdg.configHome}/nixpkgs/config.nix
    '';
}
