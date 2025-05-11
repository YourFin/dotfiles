{
  config,
  lib,
  pkgs,
  ...
}:

{
  home.packages = with pkgs; [
    pyright
    ruff
  ];
  yf.emacs.features = [ "python" ];
}
