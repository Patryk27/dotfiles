{ config, pkgs, ... }: {
  environment = {
    loginShellInit = ''
      [[ "$(tty)" == /dev/tty1 ]] && sway
    '';
  };

  security = {
    pam = {
      services = {
        swaylock = {
          text = ''
            auth include login
          '';
        };
      };
    };
  };

  home-manager.users.pwy = {
    home = {
      file = {
        ".xkb/symbols/custom".text = ''
          default partial alphanumeric_keys modifier_keys

          xkb_symbols "basic" {
            include "pl"

            key <PGUP> { [ Home ] };
            key <PGDN> { [ End ] };
            key <HOME> { [ Page_Up ] };
            key <END> { [ Page_Down ] };
          };
        '';
      };

      packages = with pkgs; [
        brightnessctl
        grim
        qt5.qtwayland
        slurp
        swaylock
        wf-recorder
        wl-clipboard
      ];
    };

    wayland = {
      windowManager = {
        sway = {
          enable = true;
          config = null;

          extraConfig = builtins.replaceStrings
            [
              "@@wallpaper@@"
            ]
            [
              (toString ./sway/wallpaper.jpg)
            ]
            (builtins.readFile ./sway/config);

          extraSessionCommands = ''
            export DOOMLOCALDIR="${config.home-manager.users.pwy.home.sessionVariables.DOOMLOCALDIR}"
            export QT_QPA_PLATFORM=wayland
            export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
            export SDL_VIDEODRIVER=wayland
          '';

          wrapperFeatures = {
            gtk = true;
          };
        };
      };
    };
  };

  nixpkgs = {
    overlays = [
      (self: super: {
        sway = super.sway.override {
          sway-unwrapped = super.sway-unwrapped.overrideAttrs (old: {
            patches = old.patches ++ [
              ./sway/patch/bottom-titlebars.patch
            ];
          });
        };
      })
    ];
  };
}
