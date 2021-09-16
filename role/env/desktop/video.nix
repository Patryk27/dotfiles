{ pkgs, ... }: {
  environment = {
    sessionVariables = {
      MOZ_ENABLE_WAYLAND = "1";
      XDG_CURRENT_DESKTOP = "sway";
      XDG_SESSION_TYPE = "wayland";
    };

    systemPackages = with pkgs; [
      gnome3.adwaita-icon-theme
      gnome3.nautilus
      gnomeExtensions.appindicator
      gsettings-desktop-schemas
      gtk-engine-murrine
      gtk_engines
      lxappearance
      okular
      pavucontrol
    ];
  };

  programs = {
    dconf = {
      enable = true;
    };

    file-roller = {
      enable = true;
    };
  };

  services = {
    gnome = {
      gnome-keyring = {
        enable = true;
      };
    };

    gvfs = {
      enable = true;
    };

    pipewire = {
      enable = true;
    };

    udev = {
      packages = with pkgs; [
        gnome3.gnome-settings-daemon
      ];
    };
  };

  xdg = {
    portal = {
      enable = true;

      extraPortals = with pkgs; [
        xdg-desktop-portal-wlr
        xdg-desktop-portal-gtk
      ];

      gtkUsePortal = true;
    };
  };
}
