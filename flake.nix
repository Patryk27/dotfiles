{
  inputs = {
    doom-emacs = {
      url = "path:/Users/pwy/Projects/doom-emacs";
      flake = false;
    };

    darwin = {
      url = "github:lnl7/nix-darwin/master";

      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
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
      url = "github:nixos/nixpkgs";
    };
  };

  outputs =
    { self
    , darwin
    , doom-emacs
    , home-manager
    , kitty-themes
    , nix
    , nixpkgs
    }:
    {
      darwinConfigurations = {
        mac = darwin.lib.darwinSystem {
          system = "aarch64-darwin";

          modules = [
            ./nodes/mac.nix

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
                      nixpkgs-x86 = import nixpkgs {
                        system = "x86_64-linux";
                      };

                    in
                    {
                      sources = {
                        inherit doom-emacs kitty-themes;
                      };

                      iosevka = nixpkgs-x86.iosevka;
                    })
                ];
              };
            })

            home-manager.darwinModules.home-manager
          ];
        };
      };
    };
}
