{ home-manager, nix-darwin, ... }@inputs:

nix-darwin.lib.darwinSystem {
  system = "aarch64-darwin";

  specialArgs = {
    inherit inputs;

    user = "pwychowaniec";
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
            pwychowaniec = {
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
            pwychowaniec = {
              home = "/Users/pwychowaniec";
            };
          };
        };
      }
    )
  ];
}
