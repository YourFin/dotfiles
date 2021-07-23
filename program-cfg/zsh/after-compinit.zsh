################################
# Begin after-compinit.sh
################################
# More history options
setopt BANG_HIST                 # Treat the '!' character specially during expansion.

setopt HIST_IGNORE_ALL_DUPS      # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY               # Don't execute immediately upon history expansion.
setopt appendhistory extendedglob

## Draw a line between commands
# Check if \u2501 is printable
if [ -z "$({ echo -n "\u2501" >/dev/null; } 2>&1)" ] ; then
    export DRAW_LINE_CHAR="\u2501"
else
    export DRAW_LINE_CHAR="-"
fi
MAX_SEPERATOR_WIDTH=60
draw_line () {
    print -Pn "%B%F{249}"
    # https://superuser.com/a/86342
    printf "$DRAW_LINE_CHAR"'%.0s' {1..$(( COLUMNS < MAX_SEPERATOR_WIDTH ? COLUMNS : MAX_SEPERATOR_WIDTH ))}
    print -Pn "%F{reset}%b\n"
}

typeset -a preexec_functions

preexec_functions+=("draw_line")

bindkey '^r' history-incremental-pattern-search-backward

# Direnv

if which "direnv" &>/dev/null ; then
  eval "$(direnv hook zsh)"
fi

# Ocaml
export OPAMROOT="$HOME/.local/usr/opam"
test -r "$HOME"/.local/usr/opam/opam-init/init.zsh && . "$HOME"/.local/usr/opam/opam-init/init.zsh >/dev/null 2>/dev/null || true

################################
# End after-compinit.sh
################################
