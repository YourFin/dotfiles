{ config, pkgs, ... }:

let 
  unstable = import <unstable> {};
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  fonts.fontconfig.enable = true;

  # pkgs.nerdfonts = pkgs.nerdfonts.override {
  #     fonts = [
  #       "FiraCode"
  #     ];
  # };
 

  home.packages = with pkgs; [
    # Graphical applications
    chromium
    discord
    keepass
    nextcloud-client
    pdfarranger
    spotify
    vlc

    # Fonts
    unstable.nerdfonts

    # Command line utils
    awscli
    bat
    exa
    fd
    fzf
    ncdu
    git
    ripgrep-all
    youtube-dl

    # Networking
    nmap
   
    # TUI
    htop
    
    # Required programming langs
    ruby
    python
    clang
  ];

  programs.emacs.enable = true;

  programs.firefox.enable = true;

  programs.kitty = {
    enable = true;
    keybindings = {
      "ctrl+shift+v" =        "paste_from_clipboard";
      "ctrl+shift+s" =        "paste_from_selection";
      "ctrl+shift+c" =        "copy_to_clipboard";
      "shift+insert" =        "paste_from_selection ";
      "ctrl+shift+t" =        "new_tab_with_cwd";
      "ctrl+shift+x" =        "close_tab";
      "ctrl+shift+h" =        "previous_tab";
      "ctrl+shift+l" =        "next_tab";
      "ctrl+shift+left" =     "move_tab_forward";
      "ctrl+shift+right" =    "move_tab_backward";
      "ctrl+shift+alt+t" =    "set_tab_title";
      "ctrl+shift+equal" =    "increase_font_size";
      "ctrl+shift+minus" =    "decrease_font_size";
      "ctrl+shift+backspace" = "restore_font_size";
      "ctrl+shift+u" =        "input_unicode_character";
    };
    settings = {
      enable_audio_bell = false;
      font_family = "FiraCode Nerd Font";
      font_size = "11.0";
      # Visuals
      background = "#000000";
      background_opacity = "0.8";
      
      allow_remote_control = "no";
    };
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.03";
}
