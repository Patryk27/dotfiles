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

            modules-left = [
              "sway/mode"
              "sway/workspaces"
            ];

            modules-center = [
              "tray"
            ];

            modules-right = [
              "custom/screen-recorder"
              "custom/mako"
              "backlight"
              "pulseaudio"
              "cpu"
              "memory"
              "custom/disk"
              "temperature"
              "battery"
              "clock"
            ];

            modules = {
              "custom/disk" = {
                exec = pkgs.writeShellScript "exec" ''
                  free=$(zpool list -H -o free rpool)
                  echo "disk[$free]"
                '';

                interval = 1;
              };

              "custom/mako" = {
                exec = pkgs.writeShellScript "exec" ''
                  makoctl set-mode default
                  echo "mako[+]"

                  dbus-monitor "interface=fr.emersion.Mako,member=SetMode" | while read event; do
                    if [[ "$event" =~ "default" ]]; then
                      echo "mako[+]"
                    fi

                    if [[ "$event" =~ "dnd" ]]; then
                      echo "mako[-]"
                    fi
                  done
                '';
              };

              "custom/screen-recorder" = {
                exec = pkgs.writeShellScript "exec" ''
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
                format = "bat[C{capacity},P{power:0.1f},+]";
                format-discharging = "bat[C{capacity},P{power:0.1f},-]";
                tooltip = false;
                interval = 1;

                states = {
                  critical = 30;
                };
              };

              backlight = {
                format = "lcd[{percent}]";
              };

              clock = {
                format = "td[{:%M:%H,%u-%d-%U-%m-%Y}]";
              };

              cpu = {
                format = "cpu[U{usage},L{load:0.1f},Fm{min_frequency:0.1f},Fa{avg_frequency:0.1f},Fx{max_frequency:0.1f}]";
                interval = 1;
              };

              memory = {
                format = "mem[{used:0.2f}]";
                interval = 1;
              };

              pulseaudio = {
                format = "vol[{volume}]";
                format-muted = "vol[{volume},-]";
                format-bluetooth = "vol[{volume},bt]";
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
