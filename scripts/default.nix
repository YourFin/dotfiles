{ lib, stdenv, coreutils, pkgs }:

let
  attrsToList = attrset:
    with builtins;
    map (k:
      let v = getAttr k attrset;
      in {
        name = k;
        value = v;
      }) (attrNames attrset);
  scriptDirPairs = builtins.filter (pair: pair.value == "directory")
    (attrsToList (builtins.readDir ./.));
  neededInterpreters = with builtins;
    intersectAttrs (listToAttrs scriptDirPairs) pkgs;
  interpreterPathSet = with builtins;
    mapAttrs (k: v: (toString v.outPath) + "/bin/") neededInterpreters;
in stdenv.mkDerivation (interpreterPathSet // {
  pname = "yourfin-shell-scripts";
  version = "0.1";
  builder = ./builder.sh;
  src = ./.;
  coreutils = pkgs.coreutils.outPath;
  meta = with lib; {
    homepage = "https://github.com/yourfin/dotfiles/scripts";
    description = "Throw away shell scripts";
    license = licenses.unlicense;
  };
})
