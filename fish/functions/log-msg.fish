function log-msg
  argparse --name log-msg --stop-nonopt \
    'n/name=' \
    'l/level=!__log-msg__validate-level' \
    -- $argv
  or return 1

  set -l name
  if set -q _flag_name
    set name "$_flag_name"
  end

  set -l level error
  if set -q _flag_level
    set level "$_flag_level"
  end

  set -l colorize
  if isatty stderr
    set colorize true
  end

  begin
    if [ -n "$colorize" ]
      set -l color_var_name log_msg_"$level"_color
      if set -q "$color_var_name"
        set_color $$color_var_name
      else
        switch "$level"
          case fatal error
            set_color red --bold
          case warning
            set_color yellow --bold
          case info
            set_color blue --bold
          case debug
            set_color green --bold
        end
      end
    end

    printf '%s:' "$level"

    if [ -n "$colorize" ]
      set_color normal
    end

    if set -l caller (caller-location "$name")
      printf ' %s:' "$caller"
    end
    if set -q _flag_name
      printf ' %s:' "$name"
    end

    printf ' %s\n' "$argv"
  end >&2
end

function __log-msg__validate-level --no-scope-shadowing
  if not contains -- "$_flag_value" fatal error warning info debug
    printf '%s: Value ‘%s’ for flag ‘%s’ is not one of ‘fatal’, ‘error’, ‘warning’, ‘info’, or ‘debug’' \
      "$_argparse_cmd" "$_flag_value" "$_flag_name"
    return 1
  end
end
