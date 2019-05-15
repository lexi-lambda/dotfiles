function set-status -d 'Set the special $status variable'
  if contains -- -h $argv; or contains -- --help $argv
    echo "usage: set-status STATUS" >&2
    return 0
  end

  set -l argc (count $argv)
  if [ "$argc" -ne 1 ]
    echo "set-status: expected exactly one argument, given $argc" >&2
    return 1
  end

  set -l new_status "$argv[1]"
  if not string match -qr -- '^-?[0-9]+$' "$new_status"
    echo "set-status: expected an integer, given" (string escape "$new_status") >&2
    return 1
  end
  return "$new_status"
end
