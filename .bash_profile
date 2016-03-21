source ~/.git-prompt.sh
source ~/.git-completion.sh

# Bash History

HISTFILESIZE=5000
HISTSIZE=5000

# Editor

export EDITOR=emacs

# Colors

test -e ~/.dircolors && \
   eval `dircolors -b ~/.dircolors`

#enables color for iTerm
export TERM=xterm-256color

#sets up proper alias commands when called
alias ls="ls --color=always"
alias grep='grep --color=always'
alias egrep='grep --color=always'
alias ll='ls -hl'
alias cp='cp -iv'
alias mv='mv -iv'
alias vi='vim'

# PS1

function prompt {
  local BLACK="\[\033[0;30m\]"
  local BLACKBOLD="\[\033[1;30m\]"
  local RED="\[\033[0;31m\]"
  local REDBOLD="\[\033[1;31m\]"
  local GREEN="\[\033[0;32m\]"
  local GREENBOLD="\[\033[1;32m\]"
  local YELLOW="\[\033[0;33m\]"
  local YELLOWBOLD="\[\033[1;33m\]"
  local BLUE="\[\033[0;34m\]"
  local BLUEBOLD="\[\033[1;34m\]"
  local PURPLE="\[\033[0;35m\]"
  local PURPLEBOLD="\[\033[1;35m\]"
  local CYAN="\[\033[0;36m\]"
  local CYANBOLD="\[\033[1;36m\]"
  local WHITE="\[\033[0;37m\]"
  local WHITEBOLD="\[\033[1;37m\]"
  export PS1="\n$WHITEBOLD\u\[\033[00m\]: $CYAN\w\[\033[00m\] $PURPLE\$(__git_ps1) \n$WHITEBOLD$\[\033[00m\] "
}

prompt

complete -C aws_completer aws

# Go stuff
export PATH=$PATH:/usr/local/go/bin
export GOPATH=$HOME/code/go
