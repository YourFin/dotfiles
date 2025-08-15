{ fetchgit, writeShellScriptBin, ... }:
# https://huggingface.co/unsloth/gpt-oss-20b-GGUF/tree/main
let
  src = fetchgit {
    fetchLFS = true;
    url = "https://huggingface.co/unsloth/gpt-oss-20b-GGUF/";
    nonConeMode = true;
    sparseCheckout = [
      "/gpt-oss-20b-F16.gguf"
      "/config.json"
      "/params"
      "/template"
      "/README.md"
    ];
    hash = "sha256-EEuBy/mpOr4Pln3JuwsCwXu00DbPzErNR7SLVdQ+W7U=";
  };
in
writeShellScriptBin "gguf-gpt-oss-20b-f16-unsloth-path" ''
  echo -n '${src}'
''
