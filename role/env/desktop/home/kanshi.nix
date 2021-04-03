{ ... }: {
  home-manager.users.pwy = { pkgs, ... }: {
    services = {
      kanshi = {
        enable = true;

        profiles = {
          anixe = {
            exec = "${pkgs.sway}/bin/swaymsg workspace 1, move workspace to DP-5, workspace 3, move workspace to DP-4";

            outputs = [
              {
                criteria = "DP-5";
                mode = "1920x1080";
                position = "0,0";
                transform = "270";
              }
              {
                criteria = "DP-4";
                mode = "1920x1080";
                position = "1920,0";
              }
              {
                criteria = "eDP-1";
                status = "disable";
              }
            ];
          };
        };
      };
    };
  };
}
