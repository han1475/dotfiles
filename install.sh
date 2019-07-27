#!/bin/sh

# Install all the necessary Arch Linux packages, especially `stow`.
sudo pacman -S \
     stow 

# Create symbolic links of dotfiles
stow emacs