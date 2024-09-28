switch:
    nix run nix-darwin -- switch --flake .

boot:
    sudo nixos-rebuild boot --flake .#fw

fmt:
    nixpkgs-fmt .
