autoload -Uz backward-kill-word-match

bindkey '^W' backward-kill-space-word
zle -N backward-kill-space-word backward-kill-word-match
zstyle :zle:backward-kill-space-word word-style space

bindkey '^[^H' backward-kill-bash-word
zle -N backward-kill-bash-word backward-kill-word-match
zstyle :zle:backward-kill-bash-word word-style bash

export PATH="$PATH:$HOME/.cargo/bin"

export GPG_TTY=$(tty)
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent

d-ip() {
    docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$@"
}

pub-add() {
    file="$1"

    if [[ -z "$file" ]]; then
        echo "usage: pub-add <path>"
        return
    fi

    fuuid=$(uuidgen | tr '[:upper:]' '[:lower:]')
    fext=$file:t:e
    fname="$fuuid.$fext"

    rsync \
        -avz \
        --rsync-path="sudo rsync" \
        "$file" \
        "gateway:/var/lib/nixos-containers/nginx/var/www/files/$fname"

    echo
    echo "https://files.pwy.io/$fname"
}

pub-ls() {
    ssh gateway -- \
        ls -l /var/lib/nixos-containers/nginx/var/www/files
}

pub-rm() {
    fname="$1"

    if [[ -z "$fname" ]]; then
        echo "usage: pub-rm <name>"
        return
    fi

    ssh gateway -- \
        sudo rm "/var/lib/nixos-containers/nginx/var/www/files/$fname"
}

z() {
    if [[ "$#" == 0 ]]; then
        fc -ln -1 | pbcopy
    else
        echo "$@" | pbcopy
    fi
}
