autoload -Uz backward-kill-word-match

bindkey '^W' backward-kill-space-word
zle -N backward-kill-space-word backward-kill-word-match
zstyle :zle:backward-kill-space-word word-style space

bindkey '^[^H' backward-kill-bash-word
zle -N backward-kill-bash-word backward-kill-word-match
zstyle :zle:backward-kill-bash-word word-style bash

export PATH="$PATH:/home/pwy/.cargo/bin"

d-ip() {
    docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$@"
}

cn-attach() {
    TERM=xterm ssh eric -t "sudo machinectl shell $1"
}

cn-down() {
    TERM=xterm ssh eric -t "sudo systemctl stop container@$1"
}

cn-restart() {
    TERM=xterm ssh eric -t "sudo systemctl stop container@$1 && sudo systemctl start container@$1"
}

cn-status() {
    TERM=xterm ssh eric -t "sudo systemctl status container@$1"
}

cn-up() {
    TERM=xterm ssh eric -t "sudo systemctl start container@$1"
}

madison-attach() {
    session="$1"

    if [[ -z "$session" ]]; then
        echo "usage: madison-attach <session>"
        return
    fi

    TERM=xterm-24bit ssh madison -t tmux attach -t "$session"
}

madison-new() {
    session="$1"

    if [[ -z "$session" ]]; then
        echo "usage: madison-new <session>"
        return
    fi

    TERM=xterm-24bit ssh madison -t tmux new -s "$session"
}

pg-rust() {
    dir=$(mktemp -d -t pg-XXXXXXXXXX)
    cd "$dir"

    cargo init --name playground

    emacs \
        -f "+vterm/here" \
        --eval '(vterm-send-string "watch -n1 cargo run")' \
        --eval "(vterm-send-return)" \
        -f "split-window-horizontally" \
        "$dir/src/main.rs" \
        -f "split-window-vertically" \
        "$dir/Cargo.toml"
}

z() {
    if [[ "$#" == 0 ]]; then
        fc -ln -1 | wl-copy -n
    else
        echo "$@" | wl-copy -n
    fi
}
