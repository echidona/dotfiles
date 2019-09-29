#!/bin/sh

DOT_DIRCTORY="$HOME/dotfiles"

ln -snfv $DOT_DIRCTORY/.config $HOME
ln -snfv $DOT_DIRCTORY/.latexmk $HOME
ln -snfv $DOT_DIRCTORY/.tmux.conf $HOME
ln -snfv $DOT_DIRCTORY/.vimrc $HOME
ln -snfv $DOT_DIRCTORY/tmux-pane-border /usr/local/bin
ln -snfv $DOT_DIRCTORY/settings.json $HOME/Library/Application\ Support/Code/User/
