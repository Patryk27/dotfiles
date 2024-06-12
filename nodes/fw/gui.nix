{ pkgs, ... }: {
  environment = {
    systemPackages = with pkgs; [
      audacity
      firefox
      gnome3.gnome-tweaks
      moonlight-qt
      slack
      spotify
    ];

    sessionVariables = {
      NIXOS_OZONE_WL = "1";
    };
  };

  services = {
    xserver = {
      enable = true;

      displayManager = {
        gdm = {
          enable = true;
          wayland = true;
        };
      };

      desktopManager = {
        gnome = {
          enable = true;
        };
      };
    };
  };
}
