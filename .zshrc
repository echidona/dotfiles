# Created by newuser for 5.7.1
export LANG=ja_JP.UTF-8
export PATH="/usr/local/opt/ncurses/bin:$PATH"
export PATH="/usr/local/opt/gettext/bin:$PATH"
export PATH="/usr/local/opt/openssl@1.1/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
# for Rust
export PATH="$HOME/.cargo/bin:$PATH"
# for vscode
export PATH="/Applications/Visual Studio Code.app/Contents/Resources/app/bin:$PATH"
# for Xwindows(Xquartz)
export DISPLAY=":0"
# for python
export PATH="/Users/kagari/Library/Python/3.7/bin:$PATH"

# brewで入れたものの補完
fpath=($(brew --prefix)/share/zsh/site-functions $fpath)
fpath+=~/.zfunc

# load for complition
autoload -U compinit && compinit -u

# 大文字と小文字を区別しない
zstyle ':completion:*' matcher-list '' 'm:{[:lower:]}={[:upper:]}' '+m:{[:upper:]}={[:lower:]}'

# VCSの情報を表示するように設定
# vcs_infoを読み込み
autoload -Uz vcs_info

#============ git の情報をプロンプトに表示する ============
# vcs_info_msg_0_変数をどのように表示するかフォーマットの指定
## デフォルトのフォーマット
### %s: どのバージョン管理システムを使っているか（git, svnなど）
### %b: ブランチ名
zstyle ':vcs_info:*' formats '(%s)[%b]'
## 特別な状態（mergeでコンフリクトしたときなど）でのフォーマット
### %a: アクション名（merge, rebaseなど）
zstyle ':vcs_info:*' actionformats '(%s)[%b|%a]'

# プロンプトが表示される毎にバージョン管理システムの情報を取得
## precmd: プロンプトが表示される毎に実行される関数
## vcs_info: バージョン管理システムから情報を取得
precmd () {
    vcs_info
    print ""
}

autoload -Uz add-zsh-hook
function save_pwd() {
    pwd > ${HOME}/.pwd
}
add-zsh-hook chpwd save_pwd
cd `cat ${HOME}/.pwd`

#============ pyenv ============
export PYENV_ROOT=/usr/local/var/pyenv
if which pyenv > /dev/null;then
    eval "$(pyenv init -)";
fi

#============ pyenv-virtualenv ============
if which pyenv-virtualenv-init > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi

 
#============ PROMPT ============
## prompt_subst: プロンプトを表示する際に変数を展開するオプション
setopt prompt_subst
## vcs_info_msg_0_: バージョン管理システムの情報
autoload -Uz colors; colors
PROMPT='%(?.%{${fg[green]}%}.%{${fg[red]}%})%c/ 》%{${reset_color}%}'
RPROMPT='${vcs_info_msg_0_}'

# load alias file
. $HOME/.zsh.d/alias.zsh

# launch tmux when start zsh
if command -v tmux &> /dev/null && [ -z "$TMUX" ]; then
#      tmux attach -t default || tmux new -s default
    exec tmux
fi
