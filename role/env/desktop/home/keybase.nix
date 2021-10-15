{ pkgs, ... }: {
  home-manager.users.pwy = {
    home = {
      packages = with pkgs; [
        keybase-gui
      ];
    };

    services = {
      keybase = {
        enable = true;
      };
    };
  };
}
