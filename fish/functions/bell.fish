function bell -d 'Print the ASCII BEL character'
  set -l previous_status "$status"

  argparse -n bell --max-args 0 'n/no-preserve-status' -- $argv
  or return 1

  printf '\a'

  if not set -q _flag_no_preserve_status
    return "$previous_status"
  end
end
