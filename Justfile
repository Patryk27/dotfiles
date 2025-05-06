host := `hostname`

switch:
    if [[ "{{ host }}" == "pfw" ]]; then \
        just switch-pfw; \
    else \
        just switch-mac; \
    fi

[private]
switch-pfw:
    sudo nixos-rebuild switch --flake .#fw

[private]
switch-mac:
    nix run nix-darwin -- switch --flake .

boot:
    if [[ "{{ host }}" == "pfw" ]]; then \
        just boot-pfw; \
    else \
        echo "not supported"; \
    fi

[private]
boot-pfw:
    sudo nixos-rebuild boot --flake .#fw

fmt:
    nixfmt .
