export PATH="$PATH:$HOME/.cargo/bin"

# ---

autoload -Uz backward-kill-word-match

bindkey '^W' backward-kill-space-word
zle -N backward-kill-space-word backward-kill-word-match
zstyle :zle:backward-kill-space-word word-style space

bindkey '^[^H' backward-kill-bash-word
zle -N backward-kill-bash-word backward-kill-word-match
zstyle :zle:backward-kill-bash-word word-style bash

# ---

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
        "gateway:/var/lib/nixos-containers/nginx/var/www/files/$fname"

    echo
    echo "https://files.pwy.io/$fname"
}

share-ls() {
    ssh gateway -- \
        ls -l /var/lib/nixos-containers/nginx/var/www/files
}

share-rm() {
    fname="$1"

    if [[ -z "$fname" ]]; then
        echo "usage: share-rm <name>"
        return
    fi

    ssh gateway -- \
        sudo rm "/var/lib/nixos-containers/nginx/var/www/files/$fname"
}

nxu() {
    if [[ -z "$1" ]]; then
        nix flake update
    else
        nix flake lock --update-input $@
    fi
}
