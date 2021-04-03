{ pkgs, ... }: {
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
        wl-clipboard
      ];
    };

    wayland = {
      windowManager = {
        sway = {
          enable = true;
          systemdIntegration = true;
          config = null;

          extraConfig =
            let
              config = builtins.readFile ./sway/config;

              vars-from = [
                "@@autorun@@"
                "@@launcher@@"
                "@@wallpaper@@"
              ];

              vars-to = [
                (toString ./sway/autorun/autorun)
                (toString ./sway/launcher)
                (toString ./sway/wallpaper.jpg)
              ];

            in
            builtins.replaceStrings vars-from vars-to config;

          extraSessionCommands = ''
            export QT_QPA_PLATFORM=wayland
            export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
            export SDL_VIDEODRIVER=wayland
            export _JAVA_AWT_WM_NONREPARENTING=1
          '';

          wrapperFeatures = {
            gtk = true;
          };
        };
      };
    };
  };
}
