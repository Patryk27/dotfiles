.PHONY: switch

switch:
	nix build .#darwinConfigurations.mac.system --option builders '' && ./result/sw/bin/darwin-rebuild switch --flake .#mac
