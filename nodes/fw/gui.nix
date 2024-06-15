{ pkgs, ... }: {
  environment = {
    systemPackages = with pkgs; [
      chromium
      easyeffects
      firefox
      gnome3.gnome-tweaks
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
