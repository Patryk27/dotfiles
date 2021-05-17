autoload -Uz backward-kill-word-match

bindkey '^W' backward-kill-space-word
zle -N backward-kill-space-word backward-kill-word-match
zstyle :zle:backward-kill-space-word word-style space

bindkey '^[^H' backward-kill-bash-word
zle -N backward-kill-bash-word backward-kill-word-match
zstyle :zle:backward-kill-bash-word word-style bash

function dip() {
    docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$@"
}

function scr-save() {
    if [[ -f "$1" ]]; then
        echo "error: file already exists: $1"
        return
    fi

    mv /tmp/screenshot.png "$1"
}
