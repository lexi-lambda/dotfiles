function with-ghc
  argparse -n with-ghc i/interactive n-no-compiler-tools -- $argv; or return 1

  set -l ghc_version $argv[1]
  set -l cmd_args $argv[2..-1]

  set -l ghc_bin_path (stack --resolver "ghc-$ghc_version" path --compiler-bin); or return 1
  set -lxp PATH (realpath $ghc_bin_path) $PATH

  if not set -q _flag_no_compiler_tools
    set -l compiler_tools_path (stack --resolver "ghc-$ghc_version" path --compiler-tools-bin); or return 1
    if test -d $compiler_tools_path
      set -p PATH $compiler_tools_path
    end
  end

  if set -q _flag_interactive
    nested-fish -l "ghc $ghc_version" -- $cmd_args
  else
    $cmd_args
  end
end
