{
  inputs = {
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

    nixpkgs-latest = {
      url = "github:nixos/nixpkgs/master";
    };
  };

  outputs =
    { self
    , home-manager
    , nixos-hardware
    , nixpkgs
    , nixpkgs-latest
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
                  (self: super:
                    let
                      nixpkgs-latest' = import nixpkgs-latest {
                        system = "x86_64-linux";
                      };

                    in
                    {
                      vue-language-server = nixpkgs-latest'.vue-language-server;
                    })
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
