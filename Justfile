all:
    just sys
    ~/.emacs.d/bin/doom sync

sys:
    just build
    just switch

build:
    nix build .#darwinConfigurations.mac.system

switch:
    ./result/sw/bin/darwin-rebuild switch --flake .#mac
