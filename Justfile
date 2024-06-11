all:
    just sys
    ~/.emacs.d/bin/doom sync

sys:
    sudo nixos-rebuild switch --flake .#fw
