{ mkDerivation, bintools }:

mkDerivation {
  name = "binutils-shadow";
  buildInputs = [ bintools ];
  installPhase = ''
    mkdir -p "$out/bin"
    for exe in '${bintools.out}/bin/'*; do
      if [ "$exe" != "ld" ] ; then
        ln -s "$exe" "$out/bin"
      fi
    done
  '';
  phases = [ "installPhase" "fixupPhase" ];
}
