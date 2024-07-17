{ pkgs, ... }: {
  environment = {
    systemPackages = with pkgs; [
      chromium
      easyeffects
      firefox
      gnome-tweaks
      wl-clipboard
    ];

    sessionVariables = {
      NIXOS_OZONE_WL = "1";
    };
  };

  services = {
    printing = {
      enable = true;

      drivers = with pkgs; [
        hplipWithPlugin
      ];
    };

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

  nixpkgs = {
    overlays = [
      (final: prev: {
        gnome = prev.gnome.overrideScope (gfinal: gprev: {
          gnome-shell = prev.gnome.gnome-shell.overrideAttrs (old: {
            patches = old.patches ++ [
              ./gui/patches/gnome-shell-disable-screenshot-notification.patch
            ];
          });
        });
      })
    ];
  };
}
