* dotfiles-and-scripts
Dotfiles and scripts providing cumbersome configure details for Arch Linux, Emacs etc. 

** Installation
*** Arch Linux
To Be Continued.
*** others
1. It won't overwrite existing files, so move those out of the way first.
2. Clone this thing wherever you like (I use ~/.dotfiles).initialize submodules in the clone,so we use ```git clone --recursive```.
```
   # initialize submodules in the clone
   git clone --recursive https://github.com/han1475/dotfiles-and-scripts.git
```
3. install GNU stow, and run the install.sh script. That'll set up a bunch of symlinks in your home directory (e.g., ~/.bashrc → ~/.dotfiles/bash/.bashrc).

*** Tips
This isn't really intended for anyone's use but my own, and it's catered to my way of doing things (duh), so, you know, be prepared for that.

Enjoy! =)