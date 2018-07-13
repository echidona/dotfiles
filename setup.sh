#!/bin/sh
# 実行場所のディレクトリを取得
DOT_FILES=$(cd $(dirname $0); pwd)
cd $DOT_FILES
echo 'start setup...'
for f in .??*; do
    [ "$f" = ".git" ] && continue
    ln -snfv ~/dotfiles/"$f" ~/
done

ln -snfv ~/dotfiles/init.el ~/.emacs.d/

cat <<EOF
DOTFILES SETUP FINISHED!
EOF
