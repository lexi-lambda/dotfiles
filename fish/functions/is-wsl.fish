function is-wsl -d 'Tests if fish is running under Windows Subsystem for Linux'
  string match -qie microsoft </proc/version
end
