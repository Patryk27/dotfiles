{ config, pkgs, ... }: {
  home-manager.users.pwy = {
    home = {
      packages = with pkgs; [
        brightnessctl
        i3lock
      ];
    };

    xsession = {
      enable = true;

      windowManager = {
        i3 = {
          enable = true;
          config = null;
          extraConfig = builtins.readFile ./i3/config;
        };
      };
    };
  };
}
