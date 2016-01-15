function fish_prompt
  set -l red       (tput setaf  1)
  set -l green     (tput setaf  2)
  set -l yellow    (tput setaf  3)
  set -l blue      (tput setaf  4)
  set -l magenta   (tput setaf  5)
  set -l cyan      (tput setaf  6)
  set -l lgray     (tput setaf  7)
  set -l gray      (tput setaf  8)
  set -l lred      (tput setaf  9)
  set -l lgreen    (tput setaf 10)
  set -l lyellow   (tput setaf 11)
  set -l lblue     (tput setaf 12)
  set -l lmagenta  (tput setaf 13)
  set -l lcyan     (tput setaf 14)
  set -l white     (tput setaf 15)
  set -l black     (tput setaf 16)
  set -l bold      (tput bold)
  set -l underline (tput smul)
  set -l reset     (tput sgr0)

  set -l location (
    if [ (pwd) = "$HOME" ]
        echo "$blue～"
    else
        set -l parent_dir (dirname (pwd))
        if [ "$parent_dir" != "$HOME" ]
          echo -ns "$blue" (basename $parent_dir) " → "
        end
        echo -s "$green" (basename (pwd))
    end
  )

  set -l question_mark_in_circle '?'\u20dd

  # show some pretty git information if we're in a git repo
  set -l git_branch (git branch ^/dev/null | sed -n '/\* /s///p')
  set -l git_prompt (
    if [ "$git_branch" ]
      # color the branch name differently if the working tree is dirty
      set -l git_color $yellow
      set -l git_status (git status --porcelain)
      set -l git_color (
        if [ "$git_status" ]; echo $lred; else; echo $yellow; end
      )
      # check how far ahead/behind the branch is
      set -l git_upstream_status (
        git rev-parse '@{u}' >/dev/null ^&1
        if [ $status -eq 0 ]
          # if an upstream is set, display information
          set -l git_commits_ahead (git rev-list '@{u}..HEAD' | wc -l | awk \{'print $1'\})
          set -l git_commits_behind (git rev-list 'HEAD..@{u}' | wc -l | awk \{'print $1'\})
          if [ \( $git_commits_ahead -eq 0 \) -a \( $git_commits_behind -eq 0 \) ]
            echo -n "$blue⦿"
          end
          if [ $git_commits_behind -gt 0 ]
            echo -n "$red↓$git_commits_behind"
          end
          if [ $git_commits_ahead -gt 0 ]
            echo -n "$lred↑$git_commits_ahead"
          end
        else
          # otherwise, indicate that an upstream is unknown
          echo "$lred$question_mark_in_circle"
        end
      )
      echo -s "$git_upstream_status $blue" '[' "$git_color$git_branch$reset$blue] "
    end
  )

  echo -s "$red# $location$blue $git_prompt$blue$lcyan" "λ:$reset "
end
