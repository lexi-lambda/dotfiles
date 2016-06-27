function start-docker
  set -l machine default
  docker-machine start $machine
  docker-machine regenerate-certs -f $machine
  attach-docker $machine
end
