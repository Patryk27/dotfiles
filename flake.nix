{
  inputs = {
    doom-emacs = {
      url = "github:Patryk27/doom-emacs/skip-lsp-during-magit-preview";
      flake = false;
    };

    emacs = {
      url = "git+https://git.savannah.gnu.org/git/emacs.git?rev=2ec77fcd8f9b4ef92ad68175c60bd85e4221bb96";
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

    # TODO https://github.com/NixOS/nixpkgs/pull/168300
    nixpkgs-anki = {
      url = "github:nixos/nixpkgs/494bd39625aed6d1da42176753750aeaef7b39e7";
    };

    nixpkgs-rust-analyzer = {
      url = "github:nixos/nixpkgs/ee3d6e2354ee612345f4f798e441b7d053ee31c9";
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
    , nixpkgs-anki
    , nixpkgs-rust-analyzer
    , sops-nix
    }:

    let
      inherit (nixpkgs) lib;

      mkNixosConfiguration = { name, system }:
        let
          pkgs-anki = import nixpkgs-anki {
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
                      inherit doom-emacs emacs;
                    };

                    anki-bin = pkgs-anki.anki-bin;
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
