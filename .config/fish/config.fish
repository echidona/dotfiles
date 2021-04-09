# start tmux when start login shell
# if status is-interactive
# and not set -q TMUX
#     exec tmux
# end

# Language
set -x LANG ja_JP.UTF-8

# ignore welcom message
set fish_greeting

# save current directory for to move automaticaly next login
function save_pre_dir --on-event fish_prompt:before
    set -U pre_dir $PWD
end

# move pre opened directory
cd $pre_dir

# command prompt
function fish_prompt
    set -l last_status $status  # save status of previous command
    # save current dir
    emit fish_prompt:before

    set -l color (set_color green)
    if [ $last_status -ne 0 ]
        # refer to color variable that defined in above
        set color (set_color red)
    end
    printf $color
    printf '%s/ \u300b' (basename $PWD)
end

function fish_right_prompt
   fish_git_prompt
end


# ---
# https://qiita.com/hoto17296/items/791936ae4e809feec7cf
function peco-docker-images
    set -l images (docker images | tail +2 | sort | peco --prompt 'DOCKER IMAGES>' | awk '{print $3}' ORS=' ')
    [ -z $images ]; and return
    commandline -i $images
end

bind \cx\ci peco-docker-images
set -g fish_user_paths "/usr/local/sbin" $fish_user_paths

# --- pipenv
# https://qiita.com/fury00812/items/08036e78a449d1cbeb48#%E3%81%8A%E3%81%BE%E3%81%91%E4%BB%AE%E6%83%B3%E7%92%B0%E5%A2%83%E3%81%AE%E7%BD%AE%E3%81%8D%E5%A0%B4%E6%89%80
if set PIPENV_VENV_IN_PROJECT; and command -v pyenv &> /dev/null
    pyenv init - | source
    set -x PIPENV_VENV_IN_PROJECT 1
end


# aliases
function maczip --wraps rm --description 'alias maczip=zip -r $argv[1]\.zip $argv -x "**.DS_Store"'
    zip -r $argv[1].zip $argv[1] -x "**.DS_Store"
end

alias emacs='emacs -nw'
alias marp-cli='docker run --rm -v $PWD:/home/marp/app/ -e LANG=$LANG marpteam/marp-cli'
