{
  inputs = {
    doom-emacs = {
      url = "github:hlissner/doom-emacs";
      flake = false;
    };

    emacs = {
      url = "git+https://git.savannah.gnu.org/git/emacs.git?rev=a0263cfee3cf64f4a77f90591af7ef7d8d78d8db"; # TODO upgrade after Emacs 29 stabilizes a bit
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

    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };

    nixpkgs-rust-analyzer = {
      url = "github:nixos/nixpkgs";
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
    , nixpkgs
    , nixpkgs-rust-analyzer
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
                      inherit doom-emacs emacs;
                    };

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
        anixe = mkNixosConfiguration {
          name = "anixe";
          system = "x86_64-linux";
        };

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

            buildInputs = with pkgs; [
              (callPackage sops-nix { }).sops-import-keys-hook

              (pkgs.writeShellScriptBin "do-deploy-madison" ''
                action="$1"

                if [[ -z "$action" ]]; then
                    action="switch"
                fi

                nixos-rebuild \
                    --flake .#madison \
                    --target-host madison \
                    --build-host madison \
                    --use-remote-sudo \
                    "$action"
              '')
            ];
          };
      };
    };
}
