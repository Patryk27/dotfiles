{
  inputs = {
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";

      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };

    home-manager = {
      url = "github:nix-community/home-manager";

      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };

    nixos-hardware = {
      url = "github:NixOS/nixos-hardware";
    };

    nixpkgs = {
      url = "github:nixos/nixpkgs/nixos-unstable";
    };
  };

  outputs =
    { self
    , emacs-overlay
    , home-manager
    , nixos-hardware
    , nixpkgs
    }:
    {
      nixosConfigurations = {
        fw = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";

          modules = [
            ./nodes/fw.nix

            ({ pkgs, ... }: {
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
                ];
              };

              system = {
                stateVersion = "24.05";
              };
            })

            home-manager.nixosModules.home-manager
            nixos-hardware.nixosModules.framework-16-7040-amd
          ];
        };
      };
    };
}
