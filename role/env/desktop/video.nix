{ pkgs, ... }: {
  environment = {
    sessionVariables = {
      MOZ_ENABLE_WAYLAND = "1";
      XDG_CURRENT_DESKTOP = "sway";
      XDG_SESSION_TYPE = "wayland";
    };

    systemPackages = with pkgs; [
      gnome3.adwaita-icon-theme
      gnome3.eog
      gnome3.evince
      gnome3.nautilus
      gnomeExtensions.appindicator
      gsettings-desktop-schemas
      gtk-engine-murrine
      gtk_engines
      lxappearance
      pavucontrol
    ];
  };

  programs = {
    dconf = {
      enable = true;
    };
  };

  services = {
    gvfs = {
      enable = true;
    };

    udev = {
      packages = with pkgs; [
        gnome3.gnome-settings-daemon
      ];
    };
  };
}
