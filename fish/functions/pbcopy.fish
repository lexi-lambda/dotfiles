function pbcopy -d 'Redirects stdin to the clipboard'
  switch (uname)
    case Darwin; command pbcopy
    case Linux
      if is-wsl; PowerShell.exe -NoProfile -Command '$Input | Set-Clipboard'
      else;      xclip -selection c; end
    case '*'
      log-msg -n pbcopy 'unknown operating system'
      return 1
  end
end
