export PAGER=vimpager
alias less=$PAGER
alias zless=$PAGER

alias grep='grep --color=auto'
alias ls='ls -p'
alias tree='tree -C'

red=$(tput setaf 1)
green=$(tput setaf 2)
yellow=$(tput setaf 3)
blue=$(tput setaf 4)
magenta=$(tput setaf 5)
cyan=$(tput setaf 6)
lgray=$(tput setaf 7)
gray=$(tput setaf 8)
lred=$(tput setaf 9)
lgreen=$(tput setaf 10)
lyellow=$(tput setaf 11)
lblue=$(tput setaf 12)
lmagenta=$(tput setaf 13)
lcyan=$(tput setaf 14)
white=$(tput setaf 15)
black=$(tput setaf 16)
bold=$(tput bold)
reset=$(tput sgr0)
export PS1='\[$lgray\][ \[$lred\]\h\[$gray\]::\[$lblue\]\u \[$lyellow\]\W \[$lgray\]] \[$reset\]'

export CLICOLOR=1
export LSCOLORS='gxfxcxdxbxegedabaggxgx'

# set node.js path
export NODE_PATH='/usr/local/lib/node_modules'

## make bash history be forever
export HISTFILESIZE=
export HISTSIZE=
export HISTTIMEFORMAT="[%F %T] "
export HISTCONTROL=ignoredups
# make sure misconfigured bash instances don't bork the history
export HISTFILE=~/.bash_eternal_history
# force the history to save after each command
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"
