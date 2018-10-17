#-------------------------------------------------#
#------------------Auto Setup---------------------#
#-------------------------------------------------#

zplugInstalled="true"

if hash git 2> /dev/null; then
    # Install zplug with curl if curl installed
    if $(! [[ -d ~/.zplug ]]) && which curl >/dev/null 2> /dev/null; then
	zplugInstalled="false"
	curl -sL --proto-redir -all,https https://raw.githubusercontent.com/zplug/installer/master/installer.zsh | zsh
    fi
else
    echo "Why isn\'t git installed. Weirdo."
fi
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
if [[ "$zplugInstalled" == "false" ]] ; then
    zplug install
    zplug update
fi

#-------------------------------------------------#
#-------------------Sources-----------------------#
#-------------------------------------------------#

source ~/.profile

#Aliases
source <(cat ~/.aliases | sed -e 's/\(.*\)#.*/\1/' | sed -e '/^$/d' | sed -e 's/^/alias /') > /dev/null

HISTFILE=~/.histfile
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

#so as not to be disturbed by Ctrl-S ctrl-Q in terminals:
stty -ixon

# You may need to manually set your language environment
export LANG=en_US.UTF-8

#prevents wm stalling
alias bspwm='bspc'

#LS
LS_COLORS='rs=0:di=01;33:ln=01;36:mh=00:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:mi=00:su=37;41:sg=30;43:ca=30;41:tw=30;42:ow=34;42:st=37;44:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.zst=01;31:*.tzst=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.jpg=01;35:*.jpeg=01;35:*.mjpg=01;35:*.mjpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;35:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.ogv=01;35:*.ogx=01;35:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.oga=00;36:*.opus=00;36:*.spx=00;36:*.xspf=00;36:';
export LS_COLORS

# Ruby?
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"

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

HISTFILE=~/.histfile

HISTSIZE=1000

SAVEHIST=1000

setopt appendhistory extendedglob

# Turn off the terminal bell in every way possible
unsetopt beep
[[ "$osType" == "Linux" ]] && xset -b

bindkey -v

# Use Freedesktop trash
function installTrashLinux()
{
    mkdir -p ~/.local/src &&
	git clone https://github.com/andreafrancia/trash-cli.git ~/.local/src/trash-cli &&
	cd ~/.local/src/trash-cli/ &&
	python setup.py install --user &&
	cd -
}
function annoyTrash()
{
    echo "WARNING: Trash-cli program not found; use rm -rv instead?"
    [[ "$osType" == "Linux" ]] &&
	$(which git >/dev/null 2>/dev/null) &&
	$(which python >/dev/null 2>/dev/null) &&
	echo "trash-cli can be installed with installTrash"
    echo "(y/N)"
    read userinput
    if [[ "$userinput" == "y" ]] ; then
	rm -rv $@
    else
	echo "Ignoring input"
    fi
}
   if $(which trash-rm > /dev/null 2>/dev/null) ; then
       alias del="trash-put"
   elif [[ "$osType" == "OSx" ]] && $(which trash > /dev/null 2>/dev/null) ; then
       alias del="trash"
   else
       [[ "$osType" == "Linux" ]] &&
	   $(which git >/dev/null 2>/dev/null) &&
	   $(which python >/dev/null 2>/dev/null) &&
	   alias installTrash="installTrashLinux"
       alias del="annoyTrash"
   fi

   # Easier pacman
   if [ -f "/etc/arch-release" ] ; then
       function y()
       {
	   if ! [ -z $(which yay) ] ; then
	       pacfunc="yay --combinedupgrade"
	   elif ! [ -z $(which trizen) ] ; then
	       pacfunc="trizen"
	   elif ! [ -z $(which pacaur) ] ; then
	       pacfunc="pacaur"
	   elif ! [ $(which yaourt) ] ; then
	       pacfunc="yaourt"
	   else
	       pacfunc="sudo pacman"
	   fi
	   if [ -z "$1" ]; then
	       eval "$pacfunc -Syu"
	   else
	       eval "$pacfunc -S $@"
	   fi &&
	       # remove to 3 versions of old packages
	       sudo paccache -r &&
	       # remove all cached uninstalled packages
	       sudo paccache -ruk0
       }
   fi

   if $(hash nvim); then
       export VISUAL='nvim'
       export EDITOR='nvim'
   else
       export VISUAL='vim'
       export EDITOR='vim'
   fi

   ### Path additions
   export PATH="$PATH:$HOME/.local/bin"
   export PATH="/usr/local/bin:$PATH"
   if $(which cargo > /dev/null 2>/dev/null) ; then
       export PATH="$PATH:$HOME/.cargo/bin"
   fi

   if $(which ruby > /dev/null 2>/dev/null) ; then
       PATH="$(ruby -e 'print Gem.user_dir')/bin:$PATH"
   fi

   autoload -U promptinit; promptinit

   #thefuck alias
   if $(which thefuck > /dev/null 2>/dev/null) ; then
       eval "$(thefuck --alias)"
   fi

   function cleanVIM()
   {
       echo "Cleaning ~/.vimbackup/"
       rm -Rf ~/.vimbackup/*
       echo "Cleaning ~/.vimswap/"
       rm -Rf ~/.vimswap/*
       echo "Cleaning ~/.vimviews/"
       rm -Rf ~/.vimviews/*
       echo "Cleaning ~/.vimundo/"
       rm -Rf ~/.vimundo/*
       echo "All done!"
   }
