function is-wsl -d 'Tests if fish is running under Windows Subsystem for Linux'
  test -f /proc/version; and string match -qie microsoft </proc/version
end
