{ config, pkgs, ... }: {
  home-manager.users.pwy = {
    home = {
      packages = with pkgs; [
        wofi
      ];
    };
  };
}
