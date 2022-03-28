{ pkgs, ... }: {
  environment = {
    sessionVariables = {
      MOZ_ENABLE_WAYLAND = "1";
      XDG_CURRENT_DESKTOP = "sway";
      XDG_SESSION_TYPE = "wayland";
    };

    systemPackages = with pkgs; [
      # Apps from https://github.com/NixOS/nixpkgs/blob/c935f5e0add2cf0ae650d072c8357533e21b0c35/nixos/modules/services/x11/desktop-managers/xfce.nix#L72
      glib
      gtk3.out
      gnome.gnome-themes-extra
      gnome.adwaita-icon-theme
      desktop-file-utils
      shared-mime-info
      polkit_gnome
      xfce.xfconf

      # Custom apps
      okular
      pavucontrol

      (xfce.thunar.override {
        thunarPlugins = [
          pkgs.xfce.thunar-archive-plugin
        ];
      })
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

  security = {
    polkit = {
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

      alsa = {
        enable = true;
        support32Bit = true;
      };

      pulse = {
        enable = true;
      };
    };

    tumbler = {
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
