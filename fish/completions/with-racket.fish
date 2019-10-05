function __with-racket__completion__has_mode
  __fish_contains_opt -s e eval -s i interactive -s l list
end
function __with-racket__completion__allows_mode
  not __with-racket__completion__has_mode
end

function __with-racket__completion__has_version
  string match -qev -- '-*' (commandline -pco | tail -n +2)
end
function __with-racket__completion__needs_version
  not __with-racket__completion__has_version
end
function __with-racket__completion__version_index
  set -l i 2
  for token in (commandline -pco | tail -n +2)
    if string match -qev -- '-*' "$token"
      echo $i
      return
    end
    set i (math $i + 1)
  end
  return 1
end

complete -c with-racket  -n __with-racket__completion__allows_mode -s e -l eval        -d 'Interpret commands using ‘eval’'
complete -c with-racket  -n __with-racket__completion__allows_mode -s i -l interactive -d 'Enter a subshell'
complete -c with-racket -xn __with-racket__completion__allows_mode -s l -l list        -d 'List available versions'
complete -c with-racket -xn __with-racket__completion__needs_version -ka '(with-racket -l)'
complete -c with-racket -xn __with-racket__completion__has_version \
  -a '(__fish_complete_subcommand --fcs-skip=(__with-racket__completion__version_index))'
