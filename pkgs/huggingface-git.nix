{
  symlinkJoin,
  fetchgit,
  writeShellScriptBin,
}:
{
  huggingface-git =
    {
      username,
      reponame,
      hash,
      models,
      extension ? "gguf",
      otherfiles ? [ "README.md" ],
    }:
    let
      src = fetchgit {
        fetchLFS = true;
        url = "https://huggingface.co/${username}/${reponame}/";
        nonConeMode = true;
        sparseCheckout = (builtins.map (m: "/${m}.${extension}") models) ++ otherfiles;
        inherit hash;
      };
    in
    symlinkJoin {
      name = "huggingface-models-${username}-${reponame}";
      paths = builtins.map (
        model:
        writeShellScriptBin "${extension}-${model}-path" ''
          echo -n '${src}/${model}.${extension}'
        ''
      ) models;
    };
}
