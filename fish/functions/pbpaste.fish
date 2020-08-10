function pbpaste -d 'Writes the contents of the clipboard to stdout'
  switch (uname)
    case Darwin; command pbpaste
    case Linux
      if is-wsl; PowerShell.exe -NoProfile -Command 'Get-Clipboard -Raw | Write-Host -NoNewline'
      else;      xclip -o -selection c; end
    case '*'
      log-msg -n pbpaste 'unknown operating system'
      return 1
  end
end
