{
  environment = {
    sessionVariables = {
      EDITOR = "vim";
    };
  };

  home-manager = {
    users = {
      root = {
        home = {
          username = "root";
          stateVersion = "24.05";
        };

        programs = {
          ssh = {
            enable = true;

            matchBlocks = {
              archive = {
                port = 33002;
                user = "pwy";
                hostname = "10.24.1.2";
                proxyJump = "gateway";
              };

              gateway = {
                hostname = "142.132.178.21";
                port = 33000;
                user = "pwy";
              };
            };
          };
        };
      };

      pwy = {
        home = {
          username = "pwy";
          stateVersion = "24.05";
        };
      };
    };
  };

  security = {
    sudo = {
      wheelNeedsPassword = false;
    };
  };

  users = {
    users = {
      pwy = {
        home = "/home/pwy";
        extraGroups = [ "docker" "libvirtd" "wheel" ];
        isNormalUser = true;
      };
    };
  };
}
