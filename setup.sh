#!/bin/bash

DOTHOME=$HOME/dotfiles

deploy() {
    for f in .??*
    do
        [[ $f =~ (.git+|.Brewfile) ]] && continue
        if [ $f == .config -a -d $HOME/.config -a ! -L $HOME/.config ] ; then
            mv $HOME/.config $HOME/.Trash
        fi
        ln -sf $DOTHOME/$f $HOME/$f
    done
}

initalize() {
    brew bundle --file=$DOTHOME/.Brewfile
}

dump() {
    brew bundle dump --file=$DOTHOME/.Brewfile --force
}

if [[ $1 = "--deploy" ]]; then
    deploy
elif [[ $1 = "--init" ]]; then
    initalize
elif [[ $1 = "--dump" ]]; then
    dump
fi
