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
      url = "github:nixos/nixpkgs/nixos-unstable";
    };

    nixpkgs-arcanist = {
      url = "github:nixos/nixpkgs";
    };

    nixpkgs-rust-analyzer = {
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
    , nixpkgs-arcanist
    , nixpkgs-rust-analyzer
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
                  (self: super: {
                    sources = {
                      inherit doom-emacs kitty-themes;
                    };

                    arcanist = (import nixpkgs-arcanist {
                      system = "aarch64-darwin";
                    }).arcanist.overrideAttrs (old: {
                      src = pkgs.fetchFromGitHub {
                        owner = "phacility";
                        repo = "arcanist";
                        rev = "e50d1bc4eabac9c37e3220e9f3fb8e37ae20b957";
                        hash = "sha256-u+HRsaCuAAyLrEihrZtLrdZ6NTVjPshieJATK3t5Fo4=";
                      };
                    });

                    rust-analyzer = (import nixpkgs-rust-analyzer {
                      system = "aarch64-darwin";
                    }).rust-analyzer;
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
