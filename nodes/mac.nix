{ home-manager, nix-darwin, ... } @ inputs:

nix-darwin.lib.darwinSystem {
  system = "aarch64-darwin";

  specialArgs = {
    inherit inputs;

    user = "pwychowaniec";
  };

  modules = [
    home-manager.darwinModules.home-manager

    ({ pkgs, ... }: {
      imports = [
        ../roles/base.nix
      ];

      environment = {
        systemPackages = with pkgs; [
          fontconfig
          redis
        ];
      };

      home-manager = {
        users = {
          pwychowaniec = {
            home = {
              stateVersion = "24.05";

              sessionVariables = {
                SSH_ASKPASS = "/usr/local/bin/ssh-askpass";
              };
            };

            programs = {
              ssh = {
                matchBlocks = {
                  builder = {
                    hostname = "10.0.0.10";
                    user = "pwychowaniec#root#10.0.9.153";
                    forwardAgent = true;
                  };

                  jail-pl-0 = {
                    hostname = "10.0.0.10";
                    user = "pwychowaniec#pwychowaniec#fudojail-pl-0";
                    forwardAgent = true;
                  };

                  jail-pl-1 = {
                    hostname = "10.0.0.10";
                    user = "pwychowaniec#pwychowaniec#fudojail-pl-1";
                    forwardAgent = true;
                  };
                };
              };
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
    })
  ];
}
