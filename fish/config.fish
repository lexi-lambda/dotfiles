if status is-login
  for dir_var in CODE_HOME DOTFILES_HOME RACKET_HOME
    set -l dir "$$dir_var"
    if [ ! -d "$dir" ]
      log-msg -l warning "value ‘$dir’ for $dir_var is not a directory"
    end
  end

  for extra_path in ~/.local/bin
    if [ -d "$extra_path" ]
      set -gxp PATH "$extra_path"
    end
  end

  set -gxp PATH "$DOTFILES_HOME"/bin "$RACKET_HOME"/bin

  set -gx PLTCOMPILEDROOTS 'compiled/@(version):'

  if type -qP vimpager
    set -gx PAGER "vimpager -c 'source ~/.vimrc'"
  end

  set -gx CLICOLOR 1
  set -gx LSCOLORS 'gxfxcxdxbxegedabaggxgx'
end

set -g fish_color_command cyan
set -g fish_color_end red
set -g fish_color_error red --bold
set -g fish_color_escape yellow
set -g fish_color_operator blue
set -g fish_color_param normal
set -g fish_color_quote green
set -g fish_color_redirection red

abbr -ag co git checkout
abbr -ag cob git checkout -b
abbr -ag gh hub browse
abbr -ag st git status -uall
