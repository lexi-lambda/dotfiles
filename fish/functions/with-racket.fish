function with-racket -d 'Parameterize the Racket version for a command'
  argparse -n with-racket -x l,e l/list e/eval -- $argv
  or return 1

  if set -q _flag_list
    if [ (count $argv) -gt 0 ]
      log-msg -n with-racket 'cannot supply arguments in --list mode'
      return 1
    end

    set -l i 1
    set -l num_versions (count $RACKET_VERSIONS)
    while [ "$i" -lt "$num_versions" ]
      printf '%s\t%s\n' $RACKET_VERSIONS["$i"..(math "$i" + 1)]
      set i (math "$i" + 2)
    end | sort -V
    return
  end

  if [ (count $argv) -lt 2 ]
    log-msg -n with-racket 'expected at least 2 arguments'
    return 1
  end

  set -lx RACKET_HOME "$RACKET_HOME"
  set -lx PATH $PATH

  begin
    set -l new_version "$argv[1]"
    set -l old_racket_bin "$RACKET_HOME"/bin

    set -l versions_left $RACKET_VERSIONS
    while [ (count $versions_left) -ge 2 ]
      set -l current_version "$versions_left[1]"
      set -l current_home "$versions_left[2]"
      if [ "$current_version" = "$new_version" ]
        set -x RACKET_HOME "$current_home"
        break
      end
      set versions_left $versions_left[3..-1]
    end

    if [ (count $versions_left) -eq 0 ]
      log-msg -n with-racket "unknown version ‘$new_version’"
      return 1
    else if [ (count $versions_left) -eq 1 ]
      log-msg -n with-racket 'malformed value for RACKET_VERSIONS (odd number of elements)'
      return 1
    end

    if set -l i (contains -i -- "$old_racket_bin" $PATH)
      set -x PATH[$i] "$RACKET_HOME"/bin
    else
      log-msg -n with-racket -l warning \
        'old value of RACKET_HOME was not in PATH; prepending new value'
      set -px PATH "$RACKET_HOME"/bin
    end
  end

  if set -q _flag_eval
    eval $argv[2..-1]
  else
    $argv[2..-1]
  end
end
