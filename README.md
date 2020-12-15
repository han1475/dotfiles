# dotfiles
Dotfiles providing cumbersome configure details for Arch Linux, Emacs, Zsh, git, etc. 
## Tips
Only tested in the following environment used by me.
- The latest version of Arch Linux without GUI in WSL2(Windows Subsystem for Linux 2)
- Windows Terminal
- Bash
## Install  
Clone this repo wherever you like (I use ~/.dotfiles).
```
  git clone  https://github.com/han1475/dotfiles.git
``` 
### Arch Linux
1. Backup packages installed by pacman to pkglist file.
```
  ./install.sh -b 
```
2. Install packages in pkglist file.
```
  ./install.sh -r
```
3. Install packages that not in pacman repository(e.g. antigen, nvm, view `install.sh` for complete list).
```
  ./insall.sh -i
```
### others
1. GNU stow won't overwrite existing files, so move those out of the way first.
2. run the install.sh script with -s option. 
   - That'll install GNU stow and set up a bunch of symlinks in your home directory(e.g., ~/.zshrc → ~/.dotfiles/bash/dot-bashrc.). 
   - I use stow(2.3.0 or later) with --dotfiles option, so "dot-" will be automatically replaced with a period(".").
```
  # only work properly in Arch Linux, since this use pacman to install GNU stow.
  ./install -s
```

