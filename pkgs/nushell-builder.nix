{
  lib,
  nushell,
  runCommandWith,
  writeTextFile,
  stdenv,
  stdenvNoCC,
}:
{
  nushell-builder =
    let
      # prevent infinite recursion for the default stdenv value
      defaultStdenv = stdenv;
      outputs' = [ "out" ]; # TODO: better support
    in
    args@{
      pname,
      stdenv ? defaultStdenv,
      nativeBuildInputs ? [ ],
      buildInputs ? [ ],
      nuVars ? { },
    }:
    text:
    let
      nativeBuildInputs' = nativeBuildInputs ++ [ nushell ];
      build-script-pkg = writeTextFile {
        name = "${pname}-builder";
        executable = true;
        destination = "/bin/build.nu";
        text = ''
          #!${nushell}/bin/nu

          ${lib.concatMapAttrsStringSep "\n" (k: v: "let ${k} = ${builtins.toJSON v}") nuVars}
          ${text}
        '';
      };
    in
    stdenv.mkDerivation ({
      inherit
        pname
        buildInputs
        ;
      name = pname;
      nativeBuildInputs = nativeBuildInputs';
      buildCommand = "${build-script-pkg}/bin/build.nu";
    });
}
