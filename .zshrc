export LANG=ja_JP.UTF-8
# PASSの設定
export PATH="/usr/local/opt/imagemagick@6/bin:$PATH"
export PATH="/usr/local/opt/mysql@5.7/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export PATH="PYENV_ROOT/bin:$PATH"

# Created by newuser for 5.5.1
# 入力したコマンドが存在せず、かつディレクトリ名と一致するなら、そのディレクトリに移動する
setopt auto_cd
DIRSTACKSIZE=100
setopt AUTO_PUSHD
autoload -Uz compinit && compinit
zstyle ':completion:*' menu select
zstyle ':completion:*:cd:*' ignore-parents parent pwd
zstyle ':completion:*:descriptions' format '%BCompleting%b %U%d%u'

# 文字コードの設定
export LANG=ja_JP.UTF-8

# 補完機能を有効にする
# 大文字小文字を区別せず補完する
autoload -U compinit; compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# コマンドの設定を保存するファイル
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

# prompt
PROMPT="%F{green}%n[%~]%f
%(?,%F{green},%F{red})%B%#%b %f"

# brewの設定やインストールしたformulaの一覧を管理できる
# caskのupgradeに便利 (-Cでクリーンアップを有効化)
# brew file cask_upgrade -C
# 下のコマンドで代用可能
# brew cask upgrade ; brew cask cleanup
# 以下のコマンドで、インストールしたcaskの数や、/Application/下にあるアプリでcaskでもインストールできるアプリの数を確認できる
## brew file casklist
if [ -f $(brew --prefix)/etc/brew-wrap ];then
    source $(brew --prefix)/etc/brew-wrap
fi

## aliasの設定
alias ll='ls -l'
alias relogin='exec $SHELL -l'
## alias emacs='emacsclient -c -a ""'
alias killemacs='emacsclient -e "(kill-emacs)"'

## pyenv/virtualenvの設定
export PYENV_ROOT="$HOME/.pyenv"
eval "$(pyenv init -)"
eval "$(pyenv virtualenv-init -)"

# pipの中身を全削除したいとき用のコマンド
pipDelete(){
    touch pip_delete_list.txt
    pip freeze > pip_delete_list.txt
    pip uninstall -r pip_delete_list.txt
    rm pip_delete_list.txt
}

# emacsでzshを使う場合、以下の設定を書く必要がある
[[ $EMACS = t ]] && unsetopt zle

# tmux
function tm(){
    if [ -n "${1}" ]; then
        tmux attach-session -t ${1} || tmux new-session -s ${1}
    else
        tmux attach-session || tmux new-session
    fi
}

function precmd() {
    if [ ! -z $TMUX ]; then
        tmux refresh-client -S
    fi
}

# plugins

