{
  inputs = {
    doom-emacs = {
      url = "path:/Users/pwy/Projects/doom-emacs";
      flake = false;
    };

    emacs = {
      url = "git+https://git.savannah.gnu.org/git/emacs.git?rev=739b5d0e52d83ec567bd61a5a49ac0e93e0eb469";
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

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
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
      url = "github:Rahix/avr-hal?dir=ravedude";
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
    };
  };

  outputs =
    { self
    , darwin
    , doom-emacs
    , emacs
    , emacs-overlay
    , home-manager
    , kitty-themes
    , nix
    , nixpkgs
    , nixpkgs-rust-analyzer
    , ravedude
    , sops-nix
    }:
    {
      darwinConfigurations = {
        mac = darwin.lib.darwinSystem {
          system = "aarch64-darwin";

          modules = [
            ./nodes/mac.nix

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
                    sources = {
                      inherit doom-emacs emacs kitty-themes;
                    };

                    rust-analyzer = (import nixpkgs-rust-analyzer { system = "aarch64-darwin"; }).rust-analyzer;
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
