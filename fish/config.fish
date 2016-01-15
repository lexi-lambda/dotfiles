
set -x PAGER "vimpager -c 'source ~/.vimrc'"

alias ls 'ls -p'
alias tree 'tree -C'
alias grep 'grep --color=auto'

set -x CLICOLOR 1
set -x LSCOLORS 'gxfxcxdxbxegedabaggxgx'

set -x fish_color_command cyan
set -x fish_color_param normal
set -x fish_color_quote green
set -x fish_color_end red
set -x fish_color_operator blue

set -x NODE_PATH '/usr/local/lib/node_modules'

# install rbenv shims
set -gx RBENV_ROOT /usr/local/var/rbenv
status --is-interactive; and . (rbenv init -|psub)

# install nvm shims
set -gx NVM_DIR=~/.nvm
