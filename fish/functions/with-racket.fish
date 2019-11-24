function with-racket -d 'Parameterize the Racket version for a command'
  argparse -n with-racket -x l,e,i l/list e/eval i/interactive -- $argv
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

  set -l min_args 2
  if set -q _flag_interactive
    set min_args 1
  end
  if [ (count $argv) -lt $min_args ]
    log-msg -n with-racket "expected at least $min_args argument(s)"
    return 1
  end

  set -lx RACKET_HOME "$RACKET_HOME"
  set -lx PATH $PATH

  set -l new_version "$argv[1]"

  begin
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
  else if set -q _flag_interactive
    nested-fish -l "racket $new_version" -- $argv[2..-1]
  else
    $argv[2..-1]
  end
end
