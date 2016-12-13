# Path to your oh-my-zsh installation.
export ZSH=/home/pen/.config/oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="agnoster"

DEFAULT_USER="pen"

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
 HYPHEN_INSENSITIVE="true"

# Uncomment the following line to enable command auto-correction.
 ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
 HIST_STAMPS="mm/dd/yyyy"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git thefuck python battery archlinux git-extras)

source $ZSH/oh-my-zsh.sh

#source etc/git-extra-completion.zsh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
if [[ -n $SSH_CONNECTION ]]; then
  export EDITOR='vim'
else
  export EDITOR='vi'
fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.


#prevents wm stalling
alias bspwm='bspc'

#so as not to be disturbed by Ctrl-S ctrl-Q in terminals:
stty -ixon

alias startmsf="sudo systemctl start postgresql.service & msfconsole --quiet -x \"db_connect ${USER}@msf\""

#thefuck alias
eval "$(thefuck --alias)"
alias gdb='gdb -tui'
alias firefox='firefox --new-window'
alias y='yaourt -Syua'
alias sudo='sudo '
alias ls='ls --color=auto'
alias la='ls --color=auto --block-size=M -la'
alias lock='/opt/scripts/fancyi3lock.sh'
alias xclipcp="xclip -selection c"
alias wifimenu="sudo wifi-menu"

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"


# post oh-my-zsh stuff
zstyle ':completion:*' completer _expand _complete _ignored
zstyle ':completion:*' glob 1
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' max-errors 1
zstyle ':completion:*' menu select=long
zstyle ':completion:*' prompt '%e errors:'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle :compinstall filename '/home/pen/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTFILE=~/.zshhistory
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory extendedglob
unsetopt beep
bindkey -v

