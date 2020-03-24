#!/bin/sh

DOTHOME=$HOME/dotfile

deploy() {
    for f in .??*
    do
        [[ $f =~ .git+ ]] && continue
        echo ln -snfv $DOTHOME/$f $HOME/$f
    done
}

initalize() {
    echo brew bundle --file=$DOTHOME/.Brewfile
}

dump() {
    echo brew bundle dump --file=$DOTHOME/.Brewfile --force
}

if [ $1 = "--deploy" -o $1 = "-d" ]; then
    deploy
elif [ $1 = "--init" -o $1 = "-i" ]; then
    initalize
elif [ $1 = "--dump" ]; then
    dump
fi
