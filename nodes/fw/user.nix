{
  environment = {
    sessionVariables = {
      EDITOR = "vim";
    };
  };

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;

    users = {
      root = {
        home = {
          username = "root";
          stateVersion = "24.05";
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
