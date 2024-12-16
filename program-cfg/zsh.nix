{ config, pkgs, ... }: {
  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    defaultKeymap = "viins";
    enableCompletion = true;
    autosuggestion.enable = true;
    enableVteIntegration = true;
    initExtraBeforeCompInit = builtins.readFile ./common-shell/early.sh
      + builtins.readFile ./zsh/before-compinit.zsh;
    # Note that we inline p10k.zsh instead of sourcing it
    initExtra = builtins.readFile ./common-shell/late.sh
      + builtins.readFile ./zsh/after-compinit.zsh
      + builtins.readFile ./zsh/p10k.zsh;

    history.ignoreSpace = true;
    history.extended = true;
    history.ignoreDups = true;
    history.path = "${config.xdg.dataHome}/zsh/zsh_history";
    history.share = true;
    plugins = [
      {
        name = "p10k";
        file = "share/zsh-powerlevel10k/powerlevel10k.zsh-theme";
        src = pkgs.zsh-powerlevel10k;
      }
      {
        name = "alias-tips";
        src = pkgs.fetchFromGitHub {
          owner = "djui";
          repo = "alias-tips";
          # Last updated 11/2021
          rev = "45e4e97ba4ec30c7e23296a75427964fc27fb029";
          sha256 = "1br0gl5jishbgi7whq4kachlcw6gjqwrvdwgk8l39hcg6gwkh4ji";
        };
      }
      {
        name = "zsh-completions";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-completions";
          rev = "0.32.0";
          sha256 = "12l9wrx0aysyj62kgp5limglz0nq73w8c415wcshxnxmhyk6sw6d";
        };
      }
      {
        name = "zsh-autosuggestions";
        src = pkgs.fetchFromGitHub {
          owner = "zsh-users";
          repo = "zsh-autosuggestions";
          rev = "v0.6.4";
          sha256 = "0h52p2waggzfshvy1wvhj4hf06fmzd44bv6j18k3l9rcx6aixzn6";
        };
      }
      {
        name = "fast-syntax-highlighting";
        src = pkgs.fetchFromGitHub {
          owner = "zdharma-continuum";
          repo = "fast-syntax-highlighting";
          rev = "9a5a4a5199e7e480009e10433d0d8c5be91f31d4";
          sha256 = "16vvmb18qny78sdrild4ksxp4776hlajps5al5b985x7zis7iqjd";
        };
      }
    ];
  };

  home.file.".zshrc".text = ''
    ## Auto generated by ${config.xdg.configHome}/nixpkgs/program-cfg/zsh.nix
    ## This file should not be edited
    source ${config.xdg.configHome}/zsh/.zshenv
    source ${config.xdg.configHome}/zsh/.zshrc
  '';
}
