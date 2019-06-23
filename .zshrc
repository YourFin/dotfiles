#-------------------------------------------------#
#-----------------Zplug---------------------------#
#-------------------------------------------------#
source ~/.zplug/init.zsh

zplug "EslamElHusseiny/aws_manager_plugin"
zplug "zpm-zsh/dropbox"
zplug "djui/alias-tips"
zplug "desyncr/auto-ls"
zplug "mafredri/zsh-async", from:github
zplug "YourFin/pure-agnoster", use:pure-agnoster.zsh, from:github, as:theme
zplug "zsh-users/zsh-autosuggestions"
zplug "srijanshetty/zsh-pip-completion"
zplug "Tarrasch/zsh-bd"

zplug load

#-------------------------------------------------#
#-------------------Sources-----------------------#
#-------------------------------------------------#

source ~/.local/opt/yf-scripts/common-shell-rc.sh

source ~/.profile

#Aliases
source <(cat ~/.aliases | sed -e 's/\(.*\)#.*/\1/' | sed -e '/^$/d' | sed -e 's/^/alias /') > /dev/null

HISTFILE=~/.local/usr/zsh/histfile
which thefuck >/dev/null 2>/dev/null && eval $(thefuck --alias)

# --------------------------- OS-specific entries ---------------------------- #
unameOut="$(uname -s)"
case "${unameOut}" in
    Linux*)     osType=Linux;;
    Darwin*)    osType=OSx;;
    CYGWIN*)    osType=Cygwin;;
    MINGW*)     osType=MinGw;;
    *)          osType="UNKNOWN:${unameOut}"
esac

#-------------------------------------------------#
#-------------------Normal Zsh--------------------#
#-------------------------------------------------#

# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _ignored _match _approximate _prefix
zstyle ':completion:*' completions 1
zstyle ':completion:*' expand prefix suffix
zstyle ':completion:*' file-sort modification
zstyle ':completion:*' format '%d'
zstyle ':completion:*' glob 1
zstyle ':completion:*' group-name ''
zstyle ':completion:*' insert-unambiguous true
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' '+l:|=* r:|=*' '+r:|[._-ABCDEFGHIJKLMNOPQRSTUVWXYZ]=** r:|=**'
zstyle ':completion:*' substitute 1
autoload -Uz compinit
compinit
# End of lines added by compinstall

# Turn off the terminal bell in every way possible
unsetopt beep
[[ "$osType" == "Linux" ]] && xset -b

bindkey -v

autoload -U promptinit; promptinit

setopt BANG_HIST                 # Treat the '!' character specially during expansion.
setopt EXTENDED_HISTORY          # Write the history file in the ":start:elapsed;command" format.
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire duplicate entries first when trimming history.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Delete old recorded entry if new entry is a duplicate.
setopt HIST_FIND_NO_DUPS         # Do not display a line previously found.
setopt HIST_IGNORE_SPACE         # Don't record an entry starting with a space.
setopt HIST_SAVE_NO_DUPS         # Don't write duplicate entries in the history file.
setopt HIST_REDUCE_BLANKS        # Remove superfluous blanks before recording entry.
setopt HIST_VERIFY               # Don't execute immediately upon history expansion.
setopt appendhistory extendedglob
HISTFILE=~/.local/usr/zsh/histfile

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
[[ -f /home/pen/gitprojects/dotfiles/config/yarn/global/node_modules/tabtab/.completions/serverless.zsh ]] && . /home/pen/gitprojects/dotfiles/config/yarn/global/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
[[ -f /home/pen/gitprojects/dotfiles/config/yarn/global/node_modules/tabtab/.completions/sls.zsh ]] && . /home/pen/gitprojects/dotfiles/config/yarn/global/node_modules/tabtab/.completions/sls.zsh
