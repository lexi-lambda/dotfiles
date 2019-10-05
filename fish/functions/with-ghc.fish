function with-ghc
  argparse -n with-ghc i/interactive -- $argv; or return 1

  set -l ghc_version $argv[1]
  set -l cmd_args $argv[2..-1]

  set -l ghc_path (stack --resolver "ghc-$ghc_version" exec -- which ghc); or return 1
  set -lx PATH (realpath (dirname $ghc_path)) $PATH
  if set -q _flag_interactive
    nested-fish -l "ghc-$ghc_version" -- $cmd_args
  else
    $cmd_args
  end
end
