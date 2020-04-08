#!/bin/sh

set -ue

###############################################################
# Local functions

# Install packages that not in pacman repository
install_packages()
{
    # install oh my zsh
    printf "%s\n" \
	   "install oh my zsh"
    curl -o- https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh | sh
    # install nvm
    printf "%s\n" \
	   "install nvm"
    curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.35.3/install.sh | sh
}
restore_packages()
{
    sudo pacman -S --needed - < pkglist
}

backup_packages()
{
    sudo pacman -Qqen > pkglist
}

create_symlink()
{
    sudo pacman -S --needed stow
    stow git
    stow emacs
    stow oh-my-zsh
}
usage()
{
    printf "%s\n" \
	   "-r, --restore   Restore packages in pkglist file"

    printf "%s\n" \
	   "-b, --backup    Backup installed by pacman packages to pkglist file"

    printf "%s\n" \
	   "-s, --symlink   Create symlink of configuration by GNU Stow"

    printf "%s\n" \
	   "-i, --install   Install packages that not in pacman repository"

    printf "%s\n" \
	   "-h, --help      Display this really usefull message"
}

###############################################################
# Command line options

if [ "$#" -eq 0 ]; then
    printf "%s\\n" "$prog_name: Argument required"
    printf "%s\\n" "Try '$prog_name --help' for more information."
    exit 1
fi

while [ "$#" -gt 0 ] ; do
    case "$1" in
	-r | --restore)
	    restore_packages
	    shift
	    ;;
	-b | --backup)
	    backup_packages
	    shift
	    ;;
	-s | --symlink)
	    create_symlink
	    shift
	    ;;
	-i | --install)
	    install_packages
	    shift
	    ;;
	-h | --help)
	    usage
	    shift
	    ;;
	*)
	    usage
	    printf "\\n%s\\n" "$prog_name: Invalid option '$1'"
	    exit 1
	    ;;
	esac
done
