# PASSの設定
export PATH="/usr/local/opt/imagemagick@6/bin:$PATH"
export PATH="/usr/local/opt/imagemagick@6/bin:$PATH"

# Created by newuser for 5.5.1
# 入力したコマンドが存在せず、かつディレクトリ名と一致するなら、そのディレクトリに移動する
setopt auto_cd

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
RPROMPT="[%d]"
PROMPT="%# "

# brewの設定やインストールしたformulaの一覧を管理できる
# caskのupgradeに便利 (-Cでクリーンアップを有効化)
# brew file cask_upgrade -C
# 下のコマンドで代用可能
# brew cask upgrade ; brew cask cleanup
# 以下のコマンドで、インストールしたcaskの数や、/Application/下にあるアプリでcaskでもインストールできるアプリの数を確認できる
# brew file casklist
if [ -f $(brew --prefix)/etc/brew-wrap ];then
    source $(brew --prefix)/etc/brew-wrap
fi

# aliasの設定
alias logisim='java -jar ~/Downloads/logisim-generic-2.7.1.jar'
alias ll='ls -l'

# 色々な設定
export PYENV_ROOT=/Users/kagari/.pyenv
export PATH=/bin:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/Library/TeX/texbin
eval export PATH="/Users/kagari/.pyenv/shims:${PATH}"
export PYENV_SHELL=zsh
source '/usr/local/Cellar/pyenv/1.2.4/libexec/../completions/pyenv.zsh'
command pyenv rehash 2>/dev/null
pyenv() {
  local command
  command="${1:-}"
  if [ "$#" -gt 0 ]; then
    shift
  fi

  case "$command" in
  rehash|shell)
    eval "$(pyenv "sh-$command" "$@")";;
  *)
    command pyenv "$command" "$@";;
  esac
}
