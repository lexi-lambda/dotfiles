if status is-login
  set -gx CODE_HOME ~/gits
  set -gx DOTFILES_HOME "$CODE_HOME"/dotfiles

  set -gx RACKET_HOME "$CODE_HOME"/racket/racket/racket
  set -gx RACKET_VERSIONS HEAD "$RACKET_HOME"

  for racket_dist_dir in '/Applications/Racket v'*
    if set -l racket_version (string match -r '[0-9.]+$' -- "$racket_dist_dir")
      set -gxa RACKET_VERSIONS "$racket_version" "$racket_dist_dir"
    end
  end

  set -l local_paths_file (dirname (status filename))/local/paths.fish
  if [ -f "$local_paths_file" ]
    source "$local_paths_file"
  end
end
