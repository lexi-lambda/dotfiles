function ls
  if isatty
    set -p argv -p
  end
  command ls $argv
end
