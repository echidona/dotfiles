#!/bin/sh

DOTHOME=$HOME/dotfile

deploy() {
    for f in .??*
    do
        [[ $f =~ (.git+|.Brewfile) ]] && continue
        echo ln -snfv $DOTHOME/$f $HOME/$f
    done
}

initalize() {
    echo brew bundle --file=$DOTHOME/.Brewfile
}

dump() {
    echo brew bundle dump --file=$DOTHOME/.Brewfile --force
}

if [ $1 = "--deploy" ]; then
    deploy
elif [ $1 = "--init" ]; then
    initalize
elif [ $1 = "--dump" ]; then
    dump
fi

# restart
exec $SHELL -l
