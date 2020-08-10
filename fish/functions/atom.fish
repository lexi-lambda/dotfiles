function atom
  if is-wsl
    # On Windows, atom.sh has carriage returns, which the WSL2 script
    # interpreter doesn’t like. So launch atom.cmd instead, converting any
    # non-flag arguments as paths.
    set -l cmd_args
    for arg in $argv
      switch $arg
        case '-*'
          set -a cmd_args $arg
        case '*'
          set -a cmd_args (wslpath -w $arg)
      end
    end
    cmd.exe /C atom.cmd $cmd_args
  else
    command atom $argv
  end
end
