{ config, pkgs, ... }: {
  home-manager.users.pwy = {
    services = {
      polybar = {
        enable = true;

        script = ''
          export PATH="$PATH:/run/current-system/sw/bin"

          {
              while ! pidof i3 >> /dev/null; do
                  sleep 1
              done

              polybar top
          } &
        '';

        package = pkgs.polybar.override {
          i3Support = true;
          pulseSupport = true;
        };

        config = {
          colors = {
            background = "#282828";
            background-alt = "#444444";

            foreground = "#dfdfdf";
            foreground-alt = "#555555";

            primary = "#ffb52a";
            secondary = "#e60053";
            alert = "#bd2c40";
          };

          settings = {
            screenchange-reload = true;
          };

          "global/wm" = {
            margin-top = 0;
            margin-bottom = 0;
          };

          "bar/top" = {
            width = "100%";
            radius = 0;
            fixed-center = false;
            enable-ipc = true;

            modules-left = "i3";

            modules-right =
              if builtins.elem "nvidia" config.services.xserver.videoDrivers then
                "cpu nvidia memory battery pulseaudio backlight temperature date"
              else
                "cpu memory battery pulseaudio backlight temperature date";

            tray-position = "right";
            tray-padding = 0;

            background = "\${colors.background}";
            foreground = "\${colors.foreground}";
            line-color = "#f00";
            border-color = "#000";

            separator = " ";

            height =
              if config.hardware.video.hidpi.enable then
                "44"
              else
                "24";

            font-0 =
              if config.hardware.video.hidpi.enable then
                "Iosevka Custom:size=18;2"
              else
                "Iosevka Custom:size=8;1";
          };

          "module/backlight" = {
            type = "internal/backlight";
            label = "led[%percentage%%]";
            card = "intel_backlight";
          };

          "module/battery" = {
            type = "internal/battery";
            battery = "BAT0";
            adapter = "AC";
            full-at = 99;
            low-at = 25;

            label-charging = "bat[^%percentage%]";
            label-discharging = "bat[%percentage%]";
            label-full = "bat[~]";
            label-low = "bat[!! %percentage% !!]";
          };

          "module/cpu" = {
            type = "internal/cpu";
            interval = 1;
            format-prefix-foreground = "\${colors.foreground-alt}";
            label = "cpu[%percentage%%]";
          };

          "module/date" = {
            type = "internal/date";
            interval = 1;

            date = "%u-%U,%d-%m-%Y";
            time = "%S:%M:%H";
            label = "cal[%time%,%date%]";
          };

          "module/i3" = {
            type = "internal/i3";
            format = "<label-state> <label-mode>";
            index-sort = true;
            wrapping-scroll = false;

            label-mode-padding = 2;
            label-mode-foreground = "#000";
            label-mode-background = "\${colors.primary}";

            label-focused = "%index%";
            label-focused-background = "\${colors.background-alt}";
            label-focused-padding = 2;

            label-unfocused = "%index%";
            label-unfocused-padding = 2;

            label-visible = "%index%";
            label-visible-background = "\${self.label-focused-background}";
            label-visible-padding = "\${self.label-focused-padding}";

            label-urgent = "%index%";
            label-urgent-background = "\${colors.alert}";
            label-urgent-padding = 2;
          };

          "module/memory" = {
            type = "internal/memory";
            interval = 1;
            format-prefix-foreground = "\${colors.foreground-alt}";
            label = "mem[%gb_used%/%gb_free%]";
          };

          "module/nvidia" = {
            type = "custom/script";
            interval = 1;

            exec = toString (pkgs.writeShellScript "nvidia" ''
              /run/current-system/sw/bin/nvidia-smi \
                  --query-gpu=utilization.gpu \
                  --format=csv,noheader,nounits \
                  | ${pkgs.gawk}/bin/awk '{ print "gpu[" $1 "%]"}'
            '');
          };

          "module/pulseaudio" = {
            type = "internal/pulseaudio";
            label-volume = "vol[%percentage%%]";
            label-muted = "vol[%percentage%%,-]";
          };

          "module/temperature" = {
            type = "internal/temperature";
            label = "temp[%temperature-c%]";
            label-warn = "temp![%temperature-c%]";
            units = true;
          };
        };
      };
    };
  };
}
