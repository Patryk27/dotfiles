{
  inputs = {
    darwin = {
      url = "github:lnl7/nix-darwin/master";

      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };

    emacs-mac = {
      url = "https://bitbucket.org/mituharu/emacs-mac/get/08a024bd93e6bd1faef9c8623dd43ce3058cfa85.zip";
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
    , emacs-mac
    , home-manager
    , kitty-themes
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
                  (self: super: {
                    sources = {
                      inherit emacs-mac kitty-themes;
                    };
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
