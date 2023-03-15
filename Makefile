.PHONY: switch

switch:
	nix build .#darwinConfigurations.mac.system && ./result/sw/bin/darwin-rebuild switch --flake .#mac
