.PHONY: all, check, switch

all:
	make switch

check:
	nix build .#darwinConfigurations.mac.system

switch:
	nix build .#darwinConfigurations.mac.system \
    && ./result/sw/bin/darwin-rebuild switch --flake .#mac
