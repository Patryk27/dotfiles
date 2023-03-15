autoload -Uz backward-kill-word-match

bindkey '^W' backward-kill-space-word
zle -N backward-kill-space-word backward-kill-word-match
zstyle :zle:backward-kill-space-word word-style space

bindkey '^[^H' backward-kill-bash-word
zle -N backward-kill-bash-word backward-kill-word-match
zstyle :zle:backward-kill-bash-word word-style bash

export PATH="$PATH:/home/pwy/.cargo/bin"

export GPG_TTY=$(tty)
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent

d-ip() {
    docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$@"
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

pub-add() {
    fpath="$1"

    if [[ -z "$fpath" ]]; then
        echo "usage: pub-add <path>"
        return
    fi

    fname=$(basename "$fpath")

    rsync \
        -avz \
        --info=progress \
        --rsync-path="sudo rsync" \
        "$fpath" \
        "eric:/var/lib/containers/nginx/var/www/files/$fname"

    echo "Ok: https://files.pwy.io/$fname"
}

pub-del() {
    fname="$1"

    if [[ -z "$fname" ]]; then
        echo "usage: pub-del <name>"
        return
    fi

    ssh eric -- \
        sudo rm "/var/lib/containers/nginx/var/www/files/$fname"
}

pub-ls() {
    ssh eric -- \
        ls -l /var/lib/containers/nginx/var/www/files
}

z() {
    if [[ "$#" == 0 ]]; then
        fc -ln -1 | xclip
    else
        echo "$@" | xclip
    fi
}
