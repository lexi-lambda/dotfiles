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

complete -c with-racket  -s e -l eval -d 'Interpret commands using ‘eval’'
complete -c with-racket -xs l -l list -d 'List available versions'
complete -c with-racket -xn __with-racket__completion__needs_version -ka '(with-racket -l)'
complete -c with-racket -xn __with-racket__completion__has_version \
  -a '(__fish_complete_subcommand --fcs-skip=(__with-racket__completion__version_index))'
