alias d='docker'
alias dr='docker run'
alias dps='docker ps'
alias dpr='docker system prune'

alias c='docker-compose'

function ce() {
    docker-compose exec -e COLUMNS="`tput cols`" -e LINES="`tput lines`" "$@"
}

function dip() {
    docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$@"
}

function drun() {
    docker run --rm -v "$(pwd):/mnt" "${@:2}" -it "$1" sh -c "cd /mnt; bash"
}
