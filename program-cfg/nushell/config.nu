alias nix-shell = nix-shell --command nu
alias cat = ^bat
alias bat = ^cat
alias firefox = firefox --new-window
alias rsync = rsync --progress
$env.config.edit_mode = 'vi';

use yf-commands.nu *

use yfnutool *
$env.config.keybindings ++= [
  { name: yfnutool_interpolate
  , modifier: CONTROL
  , keycode: Char_s
  , mode: vi_insert # Alternatively, "Emacs"
  , event:
    { send: executehostcommand
    , cmd: 'yfnutool interpolate'
    }
  }
]
