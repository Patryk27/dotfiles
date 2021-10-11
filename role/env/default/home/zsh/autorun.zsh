autoload -Uz backward-kill-word-match

bindkey '^W' backward-kill-space-word
zle -N backward-kill-space-word backward-kill-word-match
zstyle :zle:backward-kill-space-word word-style space

bindkey '^[^H' backward-kill-bash-word
zle -N backward-kill-bash-word backward-kill-word-match
zstyle :zle:backward-kill-bash-word word-style bash

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

tr-find() {
    query="$1"

    if [[ -z "$query" ]]; then
        echo "usage: tr-find <query>"
        return
    fi

    ssh eric -- \
        echo "/torrent/**/$query"
}

tr-video-play() {
    src="$1"

    if [[ -z "$src" ]]; then
        echo "usage: tr-video-play <src>"
        return
    fi

    ssh eric -- \
        nix run nixpkgs#ffmpeg -- \
            -i "${src:q}" \
            -vcodec libx265 \
            -crf 28 \
            -preset ultrafast \
            -f nut pipe:1 \
            | vlc -
}

tr-video-pull() {
    src="$1"
    dst="$2"

    if [[ -z "$src" || -z "$dst" ]]; then
        echo "usage: tr-video-pull <src> <dst>"
        return
    fi

    ssh eric -- \
        nix run nixpkgs#ffmpeg -- \
            -i "${src:q}" \
            -vcodec libx265 \
            -crf 28 \
            -preset ultrafast \
            -f nut pipe:1 \
            > "$dst"
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

ss-save() {
    output="$1"

    if [[ -z "$output" ]]; then
        echo "usage: ss-save <output>"
        return
    fi

    if [[ -f "$output" ]]; then
        echo "error: file already exists: $output"
        return
    fi

    mv /tmp/screen.png "$output"
}

sr-save() {
    output="$1"
    cut_start="$2"
    cut_len="$3"

    if [[ -z "$output" ]]; then
        echo "usage: sr-save <output> [<cut-start> <cut-len>]"
        return
    fi

    if [[ -f "$output" ]]; then
        echo "error: file already exists: $output"
        return
    fi

    pkill wf-recorder

    if [[ -z "$cut_start" ]]; then
        ffmpeg -i /tmp/screen.mp4 "$output"
    else
        ffmpeg -i /tmp/screen.mp4 -ss "$cut_start" -t "$cut_len" "$output"
    fi
}

to-gif() {
    input="$1"
    output="$2"
    fps="$3"
    scale="$4"

    if [[ -z "$input" || -z "$output" || -z "$fps" || -z "$scale" ]]; then
        echo "usage: to-gif <input> <output> <fps> <scale>"
        return
    fi

    ffmpeg \
        -y \
        -i "$input" \
        -filter_complex "fps=$fps,scale=$scale:-1:flags=lanczos[x];[x]split[x1][x2]; [x1]palettegen[p];[x2][p]paletteuse" \
        "$output"
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

rename-uuid() {
    if [[ -z "$1" ]]; then
        echo "usage: rename-uuid <file>"
        return
    fi

    for file in "$@"; do
        if [[ ! -f "$file" ]]; then
            echo "error: file doesn't exist: $file"
            return
        fi
    done

    for file in "$@"; do
        dst="$(uuidgen).${file#*.}"

        echo "$file -> $dst"
        mv "$file" "$dst"
    done
}
