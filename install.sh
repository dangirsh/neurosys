#! /usr/bin/env bash

set -vex

NEUROSYS_HOME="$(dirname "$(readlink -f "$0")")"/home

# # rsync -Pav --rsync-path="sudo rsync" nixos/ /etc/nixos/

# Cleanup old symlinks
rm -f $HOME/.doom.d
rm -f $HOME/.xmonad/xmonad.hs

ln -s $NEUROSYS_HOME/.doom.d $HOME/.doom.d
ln -s $NEUROSYS_HOME/.xmonad/xmonad.hs $HOME/.xmonad/xmonad.hs
