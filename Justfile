all:
    just system
    ~/.emacs.d/bin/doom sync

system:
    just build
    just switch

build:
    nix build .#darwinConfigurations.mac.system

switch:
    ./result/sw/bin/darwin-rebuild switch --flake .#mac
