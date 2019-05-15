function caller-location -d 'Obtain the file and line number of a function’s caller'
  set -l callee caller-location
  if [ (count $argv) -ge 1 ]
    set callee (string escape --style=regex -- "$argv[1]")
  end

  status stack-trace | while read -d \n -l entry_line
    if string match -qr "^in function '$callee'\$" -- "$entry_line"
      # this is the function we’re looking for; search for the info line
      while read -d \n -l info_line
        if [ -z "$info_line" ]
          return 1
        end
        if set -l info (string match -r 'called on line (\d+) of file (.+)$' -- "$info_line")
           and [ (count $info) -eq 3 ]
          printf '%s:%s\n' (basename "$info[3]") "$info[2]"
          return 0
        end
      end
    else
      # not what we’re looking for; skip this block
      while read -d \n -l info_line
        if [ -z "$info_line" ]
          break
        end
      end
    end
  end

  return 1
end
