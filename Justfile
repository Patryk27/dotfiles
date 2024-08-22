switch:
    sudo nixos-rebuild switch --flake .#fw

boot:
    sudo nixos-rebuild boot --flake .#fw
