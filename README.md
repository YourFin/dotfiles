# dotfiles
My general Arch dotfiles, with symlink install script

## POSIX compatible lazy install

    download=false ; if type curl >/dev/null 2>/dev/null ; then download='curl' ; elif type wget >/dev/null ; then download='wget -O -' ; fi ; eval "$download https://raw.githubusercontent.com/YourFin/dotfiles/master/install.sh" | sh

![Much pretty! Wow!](https://github.com/YourFin/dotfiles/raw/screenshots/terminalFirefox.png)
