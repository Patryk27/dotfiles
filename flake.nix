{
  inputs = {
    doom-emacs = {
      # url = "github:Patryk27/doom-emacs";
      url = "path:/home/pwy/Projects/doom-emacs";
      flake = false;
    };

    emacs = {
      url = "git+https://git.savannah.gnu.org/git/emacs.git?rev=ef77070727f262d9cecefd8a204c49a47ce4fc45";
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

    nixpkgs-rust-analyzer = {
      url = "github:nixos/nixpkgs";
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
    , nixpkgs-rust-analyzer
    , ravedude
    , sops-nix
    }:

    let
      inherit (nixpkgs) lib;

      mkNixosConfiguration = { name, system }:
        let
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
