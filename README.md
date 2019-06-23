# dotfiles
My *nix dotfiles

## POSIX compatible lazy install

    download=false ; if type curl >/dev/null 2>/dev/null ; then download='curl' ; elif type wget >/dev/null ; then download='wget -O -' ; else ; echo "No way to download dotfiles; please install curl or wget with your package manager" ; fi ; eval "$download https://raw.githubusercontent.com/YourFin/dotfiles/master/install.sh" | sh

## Arch screenshot

![Much pretty! Wow!](https://github.com/YourFin/dotfiles/raw/screenshots/terminalFirefox.png)
