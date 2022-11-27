{ stdenv, coreutils, pkgs, ... }:

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
    mapAttrs (k: v: (toString v.outPath) + "/bin/" + k) neededInterpreters;
in stdenv.mkDerivation (interpreterPathSet // {
  name = "yourfin-shell-scripts";
  builder = ./builder.sh;
  src = ./.;
  coreutils = pkgs.coreutils.outPath;
})
