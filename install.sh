#!/bin/bash

# The following will attempt to install all needed packages I usually use
# This is a quick and dirty script; there are useless error checking
# This script is meant to run on a clean fresh Arch install
#
# NOTE:
# This script assumes that Linux kernel, grub
# and base packages are installed and configured
#
# Below is a list of the packages that would be installed
#
# yay: AUR package helper
# picom: compositor for Xorg
# polybar: status bar for Xorg
# sxhkd: key bindings mapper for bspwm
# bspwm: window manager
# zwm: another window manager
# rofi: application launcher (used for window switching)
# dmenu: application launcher
# btop: CLI-based system monitor
# nemo: a graphical file manager
# alacritty: a terminal
# firefox: yeah, I use firefox
# emacs: a text editor. No it's actually another os wihin my os
# vim: another text editor
# nitrogen: wallpaper switcher
# ttf-jetbrains-mono: JetBrains Mono font
# ttf-jetbrains-mono-nerd: the nerd jt font brother 
# noto-fonts-emoji: fonts needed by the weather script in the top bar
# brightnessctl: used to control monitor brightness
# gvfs: adds missing functionality to file managers such as auto-mounting USB drives
# bluez: the bluetooth thing
# bluez-utils: command-line utilities to interact with bluetooth devices
# lxappearance: used to set GTK theme and global font
# go: go programming language
# rust: rust programming language
# cmake: build system
# clang: C and C++ compiler
# grpc: google's remote procedure call (RPC) framework
# protobuf: protocol buffers
# pavucontrol: pulseaudio volume control
# openssh: openSSH client and server
# dmidecode: DMI table decoder
# zsh: zsh shell
# amd-ucode: microcode updates for AMD processors
# arandr: front end for XRandR
# amdgpu_top: command-line tool for monitoring AMD GPU utilization
# pulseaudio: sound server, rarely works
# alsa-utils: Advanced Linux Sound Architecture (ALSA) utilities
# xf86-video-qxl: Xorg QXL video driver for virtualized environments
# xorg: Xorg display server
# xorg-xinit: Xorg initialization
# pacman-contrib: common additional tools/libs and scripts for pacman package manager
# git: I don't know what this is
# mesa: graphics library for 3D applications
# base-devel: common gevelopment tools/libs (for compiling AUR packages)
# networkmanager: network connection manager
# wpa_supplicant: wireless network authentication
# wireless_tools: tools for wireless network configuration
# netctl: network control utility
# dialog: a tool to create dialog boxes from shell scripts
# lvm2: Logical Volume Management tools
# rsync: remote file synchronization tool
# tmux: terminal multiplexer
# fzf: cli fuzzy finder
# ufw: firewall rules manger
# ripgrep: faster grep
# fd: faster find

INSTALLERDIR=$(dirname "$0")

pacman_packages=(
	picom polybar sxhkd bspwm 
	rofi dmenu btop nemo alacritty 
    firefox emacs vim nitrogen 
	brightnessctl gvfs bluez bluez-utils 
	lxappearance go rust cmake 
	clang grpc protobuf pavucontrol 
	openssh dmidecode zsh amd-ucode 
	arandr pulseaudio alsa-utils 
	xf86-video-qxl xorg xorg-xinit 
	pacman-contrib git mesa
	base-devel networkmanager wpa_supplicant
	wireless_tools netctl dialog lvm2
	rsync tmux fzf fd ripgrep
)

# install packages with pacman
echo -e "installing packages with pacman...\n"
if ! sudo pacman -S "${pacman_packages[@]}"; then
    echo -e "ERROR: pacman installation failed. Exiting script.\n"
    exit 1
fi

# start the SSH server
read -n1 -rep 'wanna start SSH server? (y,n)' HYP
if [[ $HYP == "Y" || $HYP == "y" ]]; then
	echo -e "starting SSH server...\n"
	if sudo systemctl enable --now sshd; then
    		echo -e "SSH server started successfully.\n"
    		sleep 3
	else
    		echo -e "ERROR: failed to start the SSH server. Exiting script.\n"
    		exit 1
	fi
fi

# start networkmanager
echo -e "starting networkmanager....\n"
if sudo systemctl enable --now NetworkManager; then
    echo -e "networkmanager started successfully.\n"
    sleep 3
else
    echo -e "ERROR: failed to start networkmanager. Exiting script.\n"
    exit 1
fi

echo -e "looking for yay...\n"
sleep 2
# check for yay and install if not found
if ! command -v yay &>/dev/null; then
    echo -e "yay not found. Installing yay...\n"
    git clone https://aur.archlinux.org/yay.git
    (cd yay && makepkg -si)
    # check for yay installation success
    if ! command -v yay &>/dev/null; then
        echo -e "ERROR: yay installation failed. Exiting script.\n"
        exit 1
    fi
else
    echo -e "yay is already installed. Proceeding...\n"
fi


# install AUR packages with yay
echo -e "installing AUR packages with yay...\n"
if ! yay -S \
    zwm-git ttf-jetbrains-mono ttf-jetbrains-mono-nerd \
    noto-fonts-emoji amdgpu_top vscodium-bin ttf-arabeyes-fonts gputest; then
    echo -e "ERROR: AUR package installation failed. Exiting script.\n"
    exit 1
fi

# copy config files
echo -e "Copying config files...\n"
config_dirs=(
	"$INSTALLERDIR/.config/rofi"
	"$INSTALLERDIR/.config/alacritty" 
	"$INSTALLERDIR/.config/polybar" 
	"$INSTALLERDIR/.config/bspwm" 
	"$INSTALLERDIR/.config/sxhkd" 
	"$INSTALLERDIR/.config/picom" 
	"$INSTALLERDIR/.config/btop"
	"$INSTALLERDIR/.config/emacs"
	"$INSTALLERDIR/.config/zwm"
)
for dir in "${config_dirs[@]}"; do
    if ! cp -R "$dir" $HOME/.config/; then
        echo -e "ERROR: failed to copy $dir to ~/.config/. Exiting script.\n"
        exit 1
    fi
done

# gruvbox material GTK theme and icons
echo "installing Gruvbox Material GTK theme and icons..."
# clone the Gruvbox Material GTK theme repository
if ! git clone https://github.com/TheGreatMcPain/gruvbox-material-gtk.git $HOME/gruvbox-material-gtk; then
    echo "ERROR: iailed to clone the repository. Exiting."
    exit 1
fi

# create ~/.icons and ~/.themes directories if they don't exist
echo "creating ~/.icons and ~/.themes directories..."
if ! mkdir -p $HOME/.icons $HOME/.themes; then
    echo "ERROR: failed to create directories. Exiting."
    exit 1
fi

# copy theme files to ~/.themes
if ! cp -r $HOME/gruvbox-material-gtk/themes/* $HOME/.themes; then
    echo "ERROR: failed to copy theme files. Exiting."
    exit 1
fi

# copy icon files to ~/.icons
if ! cp -r $HOME/gruvbox-material-gtk/icons/* $HOME/.icons; then
    echo "ERROR: failed to copy icon files. Exiting."
    exit 1
fi
echo "Gruvbox Material GTK theme and icons installed successfully!"

# install this (oh-my-zsh)
echo -e "installing oh-my-zsh...\n"
if ! sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"; then
    echo -e "ERROR: Oh-My-Zsh installation failed. Exiting script.\n"
    exit 1
fi

# copy the specified dot files
dot_files=("$INSTALLERDIR/.zshrc" "$INSTALLERDIR/.xinitrc" "$INSTALLERDIR/.tmux.conf")
echo -e "copying other dot files...\n"
for file in "${dot_files[@]}"; do
    if ! cp "$file" $HOME; then
        echo -e "ERROR: failed to copy $file. Exiting."
        exit 1
    fi
	if ! chmod +x $HOME/.xinitrc; then
        echo -e "ERROR: failed to make $HOME/.xinitrc executable.\n"
	fi
done


# script is done
echo -e "script is completed.\n"
echo -e "you can start zwm by executing startx\n"
# read -n1 -rep 'start now? (y,n)' HYP
# if [[ $HYP == "Y" || $HYP == "y" ]]; then
#     exec startx
# else
#     exit
# fi
