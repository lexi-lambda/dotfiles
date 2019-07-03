function pager -d 'Invoke PAGER, or invoke less if PAGER is not set'
  set -l pager
  if set -q PAGER
    set pager "$PAGER"
  else
    set pager less
  end

  # Though not specified by POSIX, tools (such as `git`, for example)
  # generally appear to interpret `PAGER` as a shell string, which
  # allows PAGER to include extra arguments to be passed to the
  # executable. For compatibility, this also uses `sh` explicitly.
  #
  # Note: we pass the value of `PAGER` itself as `$0`, which is a bit
  # funky, but it’s what `git` does, and it makes some kind of sense.
  command sh -c -- "$pager \"\$@\"" "$pager" $argv
end
