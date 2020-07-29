# All other machine configurations inherit from this one
{ config, lib, pkgs, ... }:

{
  imports = [ ../program-cfg/zsh.nix ../program-cfg/bash.nix ];
  home.packages = with pkgs; [
    bat
    exa
    fd
    fzf
    ncdu
    git
    ripgrep
    vim
    zsh

    python

    htop
  ];

  programs.direnv = {
    enable = true;
    #enableNixDirenvIntegration = true;
  };

}
