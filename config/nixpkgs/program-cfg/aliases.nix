{ config, lib, pkgs, ... }:

rec {
  aliases = {
    sudo = "sudo ";
    ls = "exa";
    la = "exa -l";
    cat = "bat";
    bat = "cat";
    restart-emacs =
      "pushd ~ ; while pkill emacs; do ; done && emacs --daemon ; popd >/dev/null";
    ec = "emacsclient";
    nix-shell = "nix-shell --command zsh";
    firefox = "firefox --new-window";
    wifimenu = "sudo wifi-menu";
  };
}
