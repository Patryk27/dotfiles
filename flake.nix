{
  inputs = {
    doom-emacs = {
      url = "github:Patryk27/doom-emacs/skip-lsp-during-magit-preview";
      flake = false;
    };

    emacs = {
      url = "git+https://git.savannah.gnu.org/git/emacs.git?rev=4266871467ebcd67ea978e28a9b9ebfccd9b4d6d";
      flake = false;
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
    };

    firenvim = {
      url = "github:glacambre/firenvim";
      flake = false;
    };

    home-manager = {
      url = "github:rycee/home-manager";

      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };

    kitty-themes = {
      url = "github:kovidgoyal/kitty-themes";
      flake = false;
    };

    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };

    # TODO https://github.com/NixOS/nixpkgs/issues/167869
    nixpkgs-openmoji = {
      url = "github:nixos/nixpkgs/ce8cbe3c01fd8ee2de526ccd84bbf9b82397a510";
    };

    nixpkgs-rust-analyzer = {
      url = "github:nixos/nixpkgs/046a25e32edad68e578b5fa3dceebbea16c887c5";
    };

    ravedude = {
      url = "github:Patryk27/avr-hal/nix?dir=ravedude";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
    };
  };

  outputs =
    { self
    , doom-emacs
    , emacs
    , emacs-overlay
    , firenvim
    , home-manager
    , kitty-themes
    , nix
    , nixpkgs
    , nixpkgs-openmoji
    , nixpkgs-rust-analyzer
    , ravedude
    , sops-nix
    }:

    let
      inherit (nixpkgs) lib;

      mkNixosConfiguration = { name, system }:
        let
          pkgs-openmoji = import nixpkgs-openmoji {
            inherit system;
          };

          pkgs-rust-analyzer = import nixpkgs-rust-analyzer {
            inherit system;
          };

        in
        lib.nixosSystem {
          inherit system;

          modules = [
            (import (./node + "/${name}.nix"))
            (import (./node + "/${name}/hardware-configuration.nix"))

            home-manager.nixosModules.home-manager
            sops-nix.nixosModules.sops

            ({ ... }: {
              nix = {
                registry = {
                  nixpkgs = {
                    flake = nixpkgs;
                  };
                };

                nixPath = [
                  "nixpkgs=${nixpkgs}"
                ];
              };

              nixpkgs = {
                overlays = [
                  emacs-overlay.overlay

                  (self: super: {
                    inherit firenvim;

                    sources = {
                      inherit doom-emacs emacs kitty-themes;
                    };

                    openmoji-color = pkgs-openmoji.openmoji-color;
                    ravedude = ravedude.defaultPackage."${system}";
                    rust-analyzer = pkgs-rust-analyzer.rust-analyzer;
                  })
                ];
              };

              sops = {
                defaultSopsFile = ./secrets.yaml;

                secrets = lib.genAttrs [
                  "backup:passphrase:anixe"
                  "backup:passphrase:lenovo"
                  "wg-fort:private-key:lenovo"
                ]
                  (k: {
                    owner = "pwy";
                  });
              };
            })
          ];
        };

    in
    {
      nixosConfigurations = {
        lenovo = mkNixosConfiguration {
          name = "lenovo";
          system = "x86_64-linux";
        };

        madison = mkNixosConfiguration {
          name = "madison";
          system = "x86_64-linux";
        };
      };

      devShell = {
        x86_64-linux =
          let
            pkgs = (import nixpkgs) {
              system = "x86_64-linux";
            };

          in
          pkgs.mkShell {
            sopsPGPKeyDirs = [
              "./key/node"
              "./key/user"
            ];

            nativeBuildInputs = with pkgs; [
              (callPackage sops-nix { }).sops-import-keys-hook

              (pkgs.writeShellScriptBin "do-deploy-madison" ''
                action="$1"

                if [[ -z "$action" ]]; then
                    action="switch"
                fi

                nixos-rebuild \
                    --flake .#madison \
                    --target-host madison \
                    --build-host localhost \
                    --use-remote-sudo \
                    "$action"
              '')
            ];
          };
      };
    };
}
