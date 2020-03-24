# to check on rm, cp, mv
alias rm='rm -i' 
alias cp='cp -i' 
alias mv='mv -i' 
# coloring ls command
alias ls='ls -GF'

# for docker build in command
alias d='docker'
alias dc='docker-compose'
alias dcnt='docker container'
alias dcur='docker container ls -f status=running -l -q'
alias dexec='docker container exec -it $(dcur)'
alias dimg='docker image'
alias drun='docker container run --rm -d'
alias drunit='docker container run --rm -it'
alias drunvit='docker container run --rm -it -v $(pwd):/root -w /root'
alias dstop='docker container stop $(dcur)'

# for latex
alias lmk='docker run --rm -v $(pwd):/root/workdir platexmk:latest latexmk'

# for runtime
alias python='drunit -v $(pwd):/root -w /root python:3.6 python'
