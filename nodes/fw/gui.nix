{ pkgs, ... }:
{
  environment = {
    gnome = {
      excludePackages = with pkgs; [
        epiphany
      ];
    };

    systemPackages = with pkgs; [
      easyeffects
      gnome-tweaks
      guvcview
      wl-clipboard
    ];

    sessionVariables = {
      MOZ_USE_XINPUT2 = "1";
    };
  };

  home-manager.users.pwy = {
    dconf = {
      settings = {
        "org/gnome/desktop/wm/keybindings" = {
          minimize = [ ];

          switch-to-workspace-up = [ "<Super>k" ];
          switch-to-workspace-down = [ "<Super>j" ];
          switch-to-workspace-left = [ "<Super>h" ];
          switch-to-workspace-right = [ "<Super>l" ];

          move-to-workspace-up = [ "<Super><Shift>k" ];
          move-to-workspace-down = [ "<Super><Shift>j" ];
          move-to-workspace-left = [ "<Super><Shift>h" ];
          move-to-workspace-right = [ "<Super><Shift>l" ];
        };
      };
    };
  };

  programs = {
    dconf = {
      enable = true;
    };
  };

  services = {
    desktopManager = {
      gnome = {
        enable = true;
      };
    };

    displayManager = {
      autoLogin = {
        enable = true;
        user = "pwy";
      };

      gdm = {
        enable = true;
      };
    };

    printing = {
      enable = true;

      drivers = with pkgs; [
        hplipWithPlugin
      ];
    };

    xserver = {
      enable = true;
    };
  };

  # https://github.com/NixOS/nixpkgs/issues/103746#issuecomment-945091229
  systemd = {
    services = {
      "getty@tty1" = {
        enable = false;
      };

      "autovt@tty1" = {
        enable = false;
      };
    };
  };

  nixpkgs = {
    overlays = [
      (final: prev: {
        gnome-shell = prev.gnome-shell.overrideAttrs (old: {
          patches = old.patches ++ [
            ./gui/patches/gnome-shell-disable-screenshot-notification.patch
          ];
        });
      })
    ];
  };
}
