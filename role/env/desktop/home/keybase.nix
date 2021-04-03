{ pkgs, ... }: {
  home-manager.users.pwy = {
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
