{ pkgs, ... }: {
  fonts = {
    fonts = with pkgs; [
      font-awesome-ttf
    ];
  };

  home-manager.users.pwy = {
    programs = {
      waybar = {
        enable = true;
        style = builtins.readFile ./waybar/style.css;

        systemd = {
          enable = true;
        };

        settings = [
          {
            layer = "top";
            position = "top";
            height = 25;

            modules-left = [ "sway/workspaces" ];
            modules-center = [ "sway/mode" ];
            modules-right = [ "custom/screen-recorder" "pulseaudio" "cpu" "memory" "temperature" "battery" "clock" "tray" ];

            modules = {
              "custom/screen-recorder" = {
                exec = pkgs.writeShellScript "script" ''
                  export PATH="$PATH:${pkgs.dbus}/bin/dbus-monitor"

                  dbus-monitor "interface=io.pwy.ScreenRecorder" | while read event; do
                    if [[ "$event" =~ "Started"  ]]; then
                      echo "*recording* "
                    fi

                    if [[ "$event" =~ "Stopped"  ]]; then
                      echo ""
                    fi
                  done
                '';
              };

              battery = {
                format = "{icon} {capacity}%";
                format-icons = [ "" "" "" "" "" ];
                tooltip = false;
                interval = 1;

                states = {
                  warning = 40;
                  critical = 20;
                };
              };

              clock = {
                format = "{:%M:%H %d-%m-%Y}";
              };

              cpu = {
                format = " {}%";
              };

              memory = {
                format = " {used:0.1f}G/{total:0.1f}G";
              };

              pulseaudio = {
                format = " {volume}%";
                format-muted = " {volume}%";
                format-bluetooth = "  {volume}%";
              };

              temperature = {
                format = " {temperatureC}°C";
                thermal-zone = 1;
                interval = 1;
              };
            };
          }
        ];
      };
    };
  };
}
