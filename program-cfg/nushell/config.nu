alias nix-shell = nix-shell --command nu
alias cat = ^bat
alias bat = ^cat
alias firefox = firefox --new-window
alias rsync = rsync --progress
$env.config.edit_mode = 'vi';

use yf-commands.nu *

use yfnutool *
use std/dirs

alias screenfetch = fastfetch

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

# TODO: unescape stuff
let fish_completer = {|spans|
    fish --command $'complete "--do-complete=($spans | str join " ")"'
      | from tsv --flexible --noheaders --no-infer
      | rename value description
      | update value {|row|
          if ($row.value | try { ($in | path exists) and ($in =~ ".*[ \\\\].*") } catch { false }) {
            $'r#"($row.value)"#'
          } else {
            $row.value
          }
      }
}

$env.config.completions.external = {
  enable: true
  completer: $fish_completer
}
