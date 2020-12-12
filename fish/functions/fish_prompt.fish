set --path -a NESTED_FISH_LABELS

function fish_prompt
  set -l command_status $status
  set -l question_mark_in_circle '?'\u20dd
  set_color red; echo -n '# '

  # print nesting
  if test (count $NESTED_FISH_LABELS) -gt 0
    echo -ns '[' (set_color yellow) \
      (string join (set_color red)/(set_color yellow) $NESTED_FISH_LABELS) \
      (set_color red) '] '
  end

  # print current location
  if [ "$PWD" = "$HOME" ]
    set_color green; echo -n '~'
  else
    set -l parent_dir (dirname "$PWD")
    set_color blue
    if [ "$parent_dir" = "$HOME" ]
      echo -n '~'
    else
      echo -n (basename "$parent_dir")
    end
    echo -n ' → '
    set_color green; echo -n (basename "$PWD")
  end
  echo -n ' '

  # show some pretty git information if we're in a git repo
  set -l is_git_worktree (git rev-parse --is-inside-work-tree 2>/dev/null)
  if test "$is_git_worktree" = true
    if git rev-parse '@{u}' >/dev/null 2>&1
      # if an upstream is set, check how far ahead/behind the branch is
      set -l git_commits_ahead (git rev-list '@{u}..HEAD' | wc -l | awk '{print $1}')
      set -l git_commits_behind (git rev-list 'HEAD..@{u}' | wc -l | awk '{print $1}')
      if [ \( "$git_commits_ahead" -eq 0 \) -a \( "$git_commits_behind" -eq 0 \) ]
        set_color blue; echo -n '⦿'
      else
        if [ "$git_commits_behind" -gt 0 ]
          set_color red; echo -n "↓$git_commits_behind"
        end
        if [ "$git_commits_ahead" -gt 0 ]
          set_color brred; echo -n "↑$git_commits_ahead"
        end
      end
    else
      # otherwise, indicate that an upstream is unknown
      set_color brred; echo -n "$question_mark_in_circle"
    end

    set -l git_branch (git branch 2>/dev/null | sed -n '/\* /s///p')
    if test -n "$git_branch"
      set_color blue; echo -n ' ['
      # color the branch name differently if the working tree is dirty
      if [ (count (git status --porcelain)) -gt 0 ]
        set_color brred
      else
        set_color yellow
      end
      echo -n "$git_branch"
      set_color blue; echo -n '] '
    end
  end

  if [ "$command_status" -eq 0 ]
    set_color brcyan; echo -n 'λ: '
  else
    set_color red; echo -n 'λ! '
  end
  set_color normal
end
