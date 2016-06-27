function attach-docker
  set -l machine default
  eval (docker-machine env $machine --shell fish)
end
