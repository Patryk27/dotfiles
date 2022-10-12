{ config, pkgs, ... }: {
  home-manager.users.pwy = {
    home = {
      packages = with pkgs; [
        brightnessctl
        feh
        i3lock
        xfce.xfce4-clipman-plugin
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
