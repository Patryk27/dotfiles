{ config, pkgs, ... }: {
  home-manager.users.pwy = { pkgs, ... }: {
    home = {
      packages = with pkgs; [
        wofi
      ];
    };
  };
}
