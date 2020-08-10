if status is-login
  set -l home_dirs $HOME
  if is-wsl
    set -p home_dirs (wslpath -a (PowerShell.exe -NoProfile Write-Host -NoNewline '$env:USERPROFILE'))
  end

  for code_dir in $home_dirs/{code,gits}
    if test -d $code_dir
      set -gx CODE_HOME $code_dir
      break
    end
  end

  set -gx DOTFILES_HOME $CODE_HOME/dotfiles

  set -gx RACKET_HOME $CODE_HOME/racket/racket/racket
  set -gx RACKET_VERSIONS HEAD $RACKET_HOME

  for racket_dist_dir in '/Applications/Racket v'*
    if set -l racket_version (string match -r '[0-9.]+$' -- $racket_dist_dir)
      set -gxa RACKET_VERSIONS $racket_version $racket_dist_dir
    end
  end

  set -l local_paths_file (dirname (status filename))/local/paths.fish
  if [ -f $local_paths_file ]
    source $local_paths_file
  end
end
