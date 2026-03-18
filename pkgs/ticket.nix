{
  jq,
  ripgrep,
  gawk,
  writeShellApplication,
  lib,
  symlinkJoin,
}:

let
  repo = builtins.fetchGit {
    url = "https://github.com/wedow/ticket.git";
    rev = "b564596e6f9ee5a9bf300600b61e1c681dc26d28";
  };
  scripts =
    lib.mapAttrsToList
      (
        k: v:
        (writeShellApplication {
          name = "ticket${k}";
          checkPhase = "";
          runtimeInputs = [
            gawk
            jq
            ripgrep
          ];
          text = builtins.readFile "${repo}/${v}";
        })
      )
      {
        "" = "ticket";
        "-ls" = "plugins/ticket-ls";
        "-edit" = "plugins/ticket-edit";
        "-query" = "plugins/ticket-query";
      };

in
{
  llm-ticket = symlinkJoin {
    name = "llm-ticket";
    paths = scripts;
  };
}
