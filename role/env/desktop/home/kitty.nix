{ pkgs, ... }: {
  home-manager.users.pwy = {
    programs = {
      kitty = {
        enable = true;

        font = {
          name = "Iosevka Custom Light";
          size = 13;
        };

        extraConfig =
          let
            kitty-themes = pkgs.fetchFromGitHub {
              owner = "dexpota";
              repo = "kitty-themes";
              rev = "fca3335489bdbab4cce150cb440d3559ff5400e2";
              hash = "sha256-DBvkVxInRhKhx5S7dzz5bcSFCf1h6A27h+lIPIXLr4U=";
            };

          in
          ''
            include ${kitty-themes}/themes/gruvbox_dark.conf
            background #1d2021

            enable_audio_bell no
            placement_strategy top-left
            clipboard_control write-clipboard write-primary read-clipboard read-primary
          '';
      };
    };
  };
}
