{
  inputs = {
    doom-emacs = {
      url = "github:hlissner/doom-emacs";
      flake = false;
    };

    emacs = {
      url = "git+https://git.savannah.gnu.org/git/emacs.git?ref=feature/pgtk";
      flake = false;
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
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

    nixpkgs-staging = {
      url = "github:nixos/nixpkgs/staging";
    };

    nixpkgs-master = {
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
    , geeqie
    , home-manager
    , nixpkgs
    , nixpkgs-staging
    , nixpkgs-master
    , sops-nix
    }:

    let
      inherit (nixpkgs) lib;

      mkNixosConfiguration = { name, system }:
        let
          pkgs-staging = import nixpkgs-staging {
            inherit system;
          };

          pkgs-master = import nixpkgs-master {
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
              };

              nixpkgs = {
                overlays = [
                  emacs-overlay.overlay

                  (self: super: {
                    sources = {
                      inherit doom-emacs emacs;
                    };

                    geeqie = super.geeqie.overrideAttrs (old: {
                      src = geeqie;

                      postPatch = ''
                        echo > doc/create-doxygen-lua-api.sh
                      '';
                    });

                    clang-tools = pkgs-staging.clang-tools;
                    rust-analyzer = pkgs-master.rust-analyzer;
                    vscode-extensions = pkgs-master.vscode-extensions;
                  })
                ];
              };

              sops = {
                defaultSopsFile = ./secrets.yaml;

                secrets = lib.genAttrs [
                  "backup-passphrase--anixe"
                  "backup-passphrase--lenovo"
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
