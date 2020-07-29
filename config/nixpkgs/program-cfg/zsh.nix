{ config, pkgs, ... }: {
  programs.zsh = {
    enable = true;
    dotDir = ".config/zsh";
    defaultKeymap = "viins";
    enableCompletion = true;
    enableAutosuggestions = true;
    initExtraBeforeCompInit = builtins.readFile ./zsh/before-compinit.zsh
      + builtins.readFile ./common-shell/early.sh;
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
        file = "powerlevel10k.zsh-theme";
        src = pkgs.fetchFromGitHub {
          owner = "romkatv";
          repo = "powerlevel10k";
          rev = "v1.12.0"; # Last updated 8/2020
          sha256 = "08zg4in70h3kray6lazszzy26gvil9w2cr6xmkbgjsv3k6w3k0jg";
          fetchSubmodules = true;
        };
      }
      {
        name = "alias-tips";
        src = pkgs.fetchFromGitHub {
          owner = "djui";
          repo = "alias-tips";
          # Last updated 8/2020
          rev = "40d8e206c6d6e41e039397eb455bedca578d2ef8";
          sha256 = "17cifxi4zbzjh1damrwi2fyhj8x0y2m2qcnwgh4i62m1vysgv9xb";
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
          owner = "zdharma";
          repo = "fast-syntax-highlighting";
          rev = "865566ce48cfd9bb5cdbaf5b1a74b0a675f4ccd4";
          sha256 = "00d6nssh73k26w69fdp8iff1xghyr8ziy5w5a2li0z9lvm6j0nik";
        };
      }
    ];
  };
}
