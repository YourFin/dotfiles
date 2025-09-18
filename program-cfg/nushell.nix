{
  config,
  lib,
  pkgs,
  ...
}:
let
  isMac = (lib.systems.elaborate builtins.currentSystem).isDarwin;
  yfnutool = pkgs.callPackage (pkgs.fetchFromGitHub {
    # Regenerate with
    # nix-prefetch-git --fetch-submodules https://github.com/YourFin/yfnutool.git
    owner = "YourFin";
    repo = "yfnutool";
    rev = "b8cceb521f7e99427f46b8b5309ce315c2611e64";
    hash = "sha256-lx16DOzGU3e5gPQa2PrVjqNfuVCZUhNJMWEHtS0E2wQ=";
    fetchSubmodules = true;
  }) { };
in
{
  home.packages = with pkgs; [
    yfnutool
  ];
  home.shell.enableNushellIntegration = true;
  programs.direnv.enableNushellIntegration = true;
  programs.nushell = {
    enable = true;
    envFile.text = ''
      ${builtins.readFile ./nushell/env.nu}${
        if isMac then "$env.NU_LIB_DIRS ++= [$'($env.HOME)/.config/nushell']\n" else ""
      }$env.NU_LIB_DIRS ++= ['${yfnutool}/share/nushell/vendor/autoload/']
      # HM session vars
      load-env ${builtins.toJSON config.home.sessionVariables}
    '';
    configFile.source = ./nushell/config.nu;
  };
  home.file.".config/nushell" = {
    recursive = true;
    source = builtins.filterSource (
      path: _type: (builtins.match ".*/(config|env)\\.nu$" path) == null
    ) ./nushell;
  };
  programs.oh-my-posh = {
    enable = true;
    enableBashIntegration = false;
    enableZshIntegration = false;
    enableNushellIntegration = true;
    settings = builtins.fromJSON ''
      {
        "$schema": "https://raw.githubusercontent.com/JanDeDobbeleer/oh-my-posh/main/themes/schema.json",
        "blocks": [
          {
            "alignment": "left",
            "segments": [
              {
                "background": "#f1184c",
                "foreground": "#242424",
                "powerline_symbol": "\ue0b0",
                "leading_powerline_symbol": "",
                "style": "powerline",
                "template": "{{ if .Root }} \uf12a {{ end }}",
                "type": "root"
              },
              {
                "background": "#242424",
                "foreground": "#FFBB00",
                "powerline_symbol": "\ue0b0",
                "leading_powerline_symbol": "",
                "properties": {
                  "time_format": "15:04:05"
                },
                "style": "powerline",
                "template": "{{ .CurrentDate | date .Format }} ",
                "type": "time"
              },
              {
                "background": "#00DD05",
                "foreground": "#000000",
                "style": "powerline",
                "powerline_symbol": "\ue0b0",
                "leading_powerline_symbol": "",
                "properties": {
                  "folder_separator_icon": "/",
                  "max_width": 40,
                  "style": "powerlevel"
                },
                "type": "path"
              },
              {
                "background": "#242424",
                "foreground": "#3A86FF",
                "powerline_symbol": "\ue0b0",
                "leading_powerline_symbol": "",
                "properties": {
                  "fetch_stash_count": true,
                  "fetch_status": true,
                  "fetch_upstream_icon": true
                },
                "style": "powerline",
                "template": " {{ .UpstreamIcon }}{{ .HEAD }}{{ if .Staging.Changed }} \uf046 {{ .Staging.String }}{{ end }}{{ if and (.Working.Changed) (.Staging.Changed) }} |{{ end }}{{ if .Working.Changed }} \uf044 {{ .Working.String }}{{ end }}{{ if gt .StashCount 0 }} \ueb4b {{ .StashCount }}{{ end }} ",
                "type": "git"
              },
              {
                "background": "#0184bc",
                "foreground": "#ffffff",
                "style": "powerline",
                "powerline_symbol": "\ue0b0",
                "leading_powerline_symbol": "",
                "template": " \ue70c {{ if .Unsupported }}\uf071{{ else }}{{ .Full }}{{ end }} ",
                "type": "dotnet"
              },
              {
                "background": "#8800dd",
                "foreground": "#ffffff",
                "powerline_symbol": "\ue0b0",
                "leading_powerline_symbol": "",
                "style": "powerline",
                "properties": {
                  "style": "austin",
                  "threshold": 750
                },
                "style": "powerline",
                "template": " <#fefefe>\ueba2</> {{ .FormattedMs }} ",
                "type": "executiontime"
              },
              {
                "background": "#33DD2D",
                "background_templates": [
                  "{{ if gt .Code 0 }}#f1184c{{ end }}"
                ],
                "foreground": "#242424",
                "template": "{{ .Code }}",
                "powerline_symbol": "\ue0b0",
                "leading_powerline_symbol": "",
                "style": "powerline",
                "type": "status"
              },
              {
                "style": "plain",
                "template": "\n",
                "type": "text"
              }
            ],
            "type": "prompt"
          }
        ],
        "console_title_template": "{{if .Root}}(Admin) {{end}}{{.Folder}}",
        "version": 3
      }
    '';
  };
}
