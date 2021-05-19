autoload -Uz backward-kill-word-match

bindkey '^W' backward-kill-space-word
zle -N backward-kill-space-word backward-kill-word-match
zstyle :zle:backward-kill-space-word word-style space

bindkey '^[^H' backward-kill-bash-word
zle -N backward-kill-bash-word backward-kill-word-match
zstyle :zle:backward-kill-bash-word word-style bash

function d-ip() {
    docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$@"
}

function madison-attach {
    session="$1"

    if [[ -z "$session" ]]; then
        echo "usage: madison-attach <session>"
        return
    fi

    TERM=xterm-24bit ssh madison -t tmux attach -t "$session"
}

function madison-new {
    session="$1"

    if [[ -z "$session" ]]; then
        echo "usage: madison-new <session>"
        return
    fi

    TERM=xterm-24bit ssh madison -t tmux new -s "$session"
}

function src-save() {
    file="$1"

    if [[ -z "$file" ]]; then
        echo "usage: scr-save <file>"
        return
    fi

    if [[ -f "$file" ]]; then
        echo "error: file already exists: $file"
        return
    fi

    mv /tmp/screenshot.png "$file"
}

function pg-rust {
    dir=$(mktemp -d -t pg-XXXXXXXXXX)
    cd "$dir"

    cargo init --name playground

    emacs -nw \
        -f "+vterm/here" \
        --eval '(vterm-send-string "watch -n1 cargo run")' \
        --eval "(vterm-send-return)" \
        -f "split-window-horizontally" \
        "$dir/src/main.rs" \
        -f "split-window-vertically" \
        "$dir/Cargo.toml"
}
