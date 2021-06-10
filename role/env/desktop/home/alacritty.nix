{ pkgs, ... }: {
  home-manager.users.pwy = {
    programs = {
      alacritty = {
        enable = true;

        settings = {
          colors = {
            primary = {
              background = "#1d2021";
              foreground = "#ebdbb2";
            };

            normal = {
              black = "#282828";
              red = "#cc241d";
              green = "#98971a";
              yellow = "#d79921";
              blue = "#458588";
              magenta = "#b16286";
              cyan = "#689d6a";
              white = "#a89984";
            };

            bright = {
              black = "#928374";
              red = "#fb4934";
              green = "#b8bb26";
              yellow = "#fabd2f";
              blue = "#83a598";
              magenta = "#d3869b";
              cyan = "#8ec07c";
              white = "#ebdbb2";
            };
          };

          font = {
            size = 13;

            normal = {
              family = "Iosevka Custom";
              style = "Light";
            };
          };
        };
      };
    };
  };
}
