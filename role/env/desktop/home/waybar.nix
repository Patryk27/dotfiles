{ pkgs, ... }: {
  fonts = {
    fonts = with pkgs; [
      fira-code
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
            modules-right = [ "pulseaudio" "cpu" "memory" "temperature" "battery" "clock" "tray" ];

            modules = {
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
