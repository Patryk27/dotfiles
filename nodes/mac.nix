{ home-manager, nix-darwin, ... }@inputs:

nix-darwin.lib.darwinSystem {
  system = "aarch64-darwin";

  specialArgs = {
    inherit inputs;
  };

  modules = [
    home-manager.darwinModules.home-manager

    (
      { pkgs, ... }:
      {
        imports = [
          ../roles/base.nix
        ];

        environment = {
          systemPackages = with pkgs; [
            fontconfig
          ];
        };

        home-manager = {
          users = {
            pwy = {
              home = {
                stateVersion = "24.05";
              };
            };
          };
        };

        services = {
          nix-daemon = {
            enable = true;
          };
        };

        system = {
          stateVersion = 5;
        };

        users = {
          users = {
            pwy = {
              home = "/Users/pwy";
            };
          };
        };
      }
    )
  ];
}
