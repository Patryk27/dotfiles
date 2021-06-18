{
  inputs = {
    doom-emacs = {
      url = "github:hlissner/doom-emacs";
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
    };

    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
    };
  };

  outputs =
    { self
    , doom-emacs
    , emacs-overlay
    , firenvim
    , home-manager
    , nixpkgs
    , sops-nix
    }:

    let
      nodes = {
        anixe = {
          system = "x86_64-linux";
        };

        lenovo = {
          system = "x86_64-linux";
        };

        madison = {
          system = "x86_64-linux";
        };
      };

      buildCheck = name: config:
        self.nixosConfigurations."${name}".config.system.build.toplevel;

      buildSystem = name: config: nixpkgs.lib.nixosSystem {
        system = config.system;

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
            };

            nixpkgs = {
              overlays = [
                emacs-overlay.overlay

                (self: super: {
                  inherit doom-emacs firenvim;
                })
              ];
            };

            sops = {
              defaultSopsFile = ./secrets.yaml;

              secrets = {
                backup-passphrase--anixe = { };
                backup-passphrase--lenovo = { };
                sccache-endpoint = { };
                sccache-key = { };
              };
            };
          })
        ];
      };

    in
    {
      checks = {
        "x86_64-linux" = builtins.mapAttrs buildCheck nodes;
      };

      nixosConfigurations = builtins.mapAttrs buildSystem nodes;

      devShell = {
        "x86_64-linux" =
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
              (callPackage sops-nix { }).sops-pgp-hook

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
