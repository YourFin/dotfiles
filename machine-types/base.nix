# All other machine configurations inherit from this one
{ config, lib, pkgs, ... }:

{
  imports =
    [ ../program-cfg/zsh.nix ../program-cfg/bash.nix ../program-cfg/vim.nix ];
  home.packages = with pkgs; [
    bat
    (lib.setPrio 100 binutils)
    exa
    fd
    fzf
    git
    iftop
    mosh
    ncdu
    ripgrep
    inetutils
    vim
    zsh

    htop
  ];

  home.file.".config" = {
    source = ../share/dot/config;
    recursive = true;
  };

  home.file.".inputrc".source = ../share/dot/inputrc;
  home.file.".gitconfig".source = ../share/dot/gitconfig;

  programs.direnv = {
    enable = true;
    #enableNixDirenvIntegration = true;
  };

}
