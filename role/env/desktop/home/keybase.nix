{ ... }: {
  home-manager.users.pwy = { pkgs, ... }: {
    home = {
      packages = with pkgs; [
        keybase-gui
      ];
    };

    services = {
      kbfs = {
        enable = true;
      };

      keybase = {
        enable = true;
      };
    };
  };
}
