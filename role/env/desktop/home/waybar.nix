{ pkgs, ... }: {
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
            position = "bottom";
            height = 25;

            modules-left = [ "sway/workspaces" ];
            modules-center = [ "sway/mode" ];
            modules-right = [ "tray" "custom/screen-recorder" "pulseaudio" "cpu" "memory" "temperature" "battery" "clock" ];

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
                format = "bat[{capacity}]";
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
                format = "cpu[{usage}]";
                interval = 1;
              };

              memory = {
                format = "mem[{used:0.2f}]";
              };

              pulseaudio = {
                format = "vol[{volume}]";
                format-muted = "vol[{volume}]";
                format-bluetooth = "vol[{volume}]";
              };

              temperature = {
                format = "temp[{temperatureC}]";
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
