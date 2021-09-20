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

    # The newest version supports Wayland, but hasn't been released yet.
    # TODO revisit when geeqie gets a release
    geeqie = {
      url = "github:BestImageViewer/geeqie";
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

    sops-nix = {
      url = "github:Mic92/sops-nix";
    };
  };

  outputs =
    { self
    , doom-emacs
    , emacs-overlay
    , firenvim
    , geeqie
    , home-manager
    , nixpkgs
    , sops-nix
    }:

    let
      inherit (nixpkgs) lib;

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

      buildSystem = name: config: lib.nixosSystem {
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

                  geeqie = super.geeqie.overrideAttrs (superAttrs: {
                    src = geeqie;

                    postPatch = ''
                      echo > doc/create-doxygen-lua-api.sh
                    '';
                  });
                })
              ];
            };

            sops = {
              defaultSopsFile = ./secrets.yaml;

              secrets = lib.genAttrs [
                "backup-passphrase--anixe"
                "backup-passphrase--lenovo"
                "sccache-endpoint"
                "sccache-key"
                "taskd-certificate"
                "taskd-ca"
                "taskd-key"
                "taskrc"
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
      checks = {
        x86_64-linux = builtins.mapAttrs buildCheck nodes;
      };

      nixosConfigurations = builtins.mapAttrs buildSystem nodes;

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
