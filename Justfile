_:
    just build
    just switch

full:
    just _
    ~/.emacs.d/bin/doom sync

build:
    nix build .#darwinConfigurations.mac.system

switch:
    ./result/sw/bin/darwin-rebuild switch --flake .#mac
