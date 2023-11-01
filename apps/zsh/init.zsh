autoload -Uz backward-kill-word-match

bindkey '^W' backward-kill-space-word
zle -N backward-kill-space-word backward-kill-word-match
zstyle :zle:backward-kill-space-word word-style space

bindkey '^[^H' backward-kill-bash-word
zle -N backward-kill-bash-word backward-kill-word-match
zstyle :zle:backward-kill-bash-word word-style bash

export PATH="$PATH:$HOME/Applications"
export PATH="$PATH:$HOME/.cargo/bin"
export GPG_TTY=$(tty)

gpgconf --launch gpg-agent

d-ip() {
    docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' "$@"
}

share() {
    file="$1"

    if [[ -z "$file" ]]; then
        echo "usage: share <path>"
        return
    fi

    fuuid=$(uuidgen | tr '[:upper:]' '[:lower:]')
    fext=$file:t:e
    fname="$fuuid.$fext"

    rsync \
        -avz \
        --info=progress2 \
        --rsync-path="sudo rsync" \
        "$file" \
        "gateway:/var/lib/nixos-containers/nginx/var/www/share/$fname"

    echo
    echo "https://share.pwy.io/$fname"
}

share-ls() {
    ssh gateway -- \
        ls -l /var/lib/nixos-containers/nginx/var/www/share
}

share-rm() {
    fname="$1"

    if [[ -z "$fname" ]]; then
        echo "usage: share-rm <name>"
        return
    fi

    ssh gateway -- \
        sudo rm "/var/lib/nixos-containers/nginx/var/www/share/$fname"
}

z() {
    if [[ "$#" == 0 ]]; then
        fc -ln -1 | pbcopy
    else
        echo "$@" | pbcopy
    fi
}
