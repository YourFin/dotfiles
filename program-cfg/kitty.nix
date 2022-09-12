{ config, lib, pkgs, ... }:

{
  programs.kitty = {
    enable = true;
    keybindings = {
      "ctrl+shift+v" = "paste_from_clipboard";
      "ctrl+shift+s" = "paste_from_selection";
      "ctrl+shift+c" = "copy_to_clipboard";
      "shift+insert" = "paste_from_selection ";
      "ctrl+shift+t" = "new_tab_with_cwd";
      "ctrl+shift+x" = "close_tab";
      "ctrl+shift+h" = "previous_tab";
      "ctrl+shift+l" = "next_tab";
      "ctrl+shift+left" = "move_tab_forward";
      "ctrl+shift+right" = "move_tab_backward";
      "ctrl+shift+alt+t" = "set_tab_title";
      "ctrl+shift+equal" = "increase_font_size";
      "ctrl+shift+minus" = "decrease_font_size";
      "ctrl+shift+backspace" = "restore_font_size";
      "ctrl+shift+u" = "input_unicode_character";
    };

    extraConfig = ''
      macos_option_as_alt yes
      macos_quit_when_last_window_closed yes
    '';
    settings = {
      allow_remote_control = "no";
      enable_audio_bell = false;
      strip_trailing_spaces = "smart";

      # Visuals
      background = "#000000";
      background_opacity = "0.8";
      font_family = "FiraCode Nerd Font";
      font_size = "11.0";
      tab_bar_edge = "top";
      tab_bar_style = "separator";
      shell_integration = "no-rc";
    };
  };
}
