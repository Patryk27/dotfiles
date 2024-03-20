{ pkgs, ... }: {
  services = {
    yabai = {
      enable = true;

      package = pkgs.yabai.overrideAttrs (old: {
        src = pkgs.fetchzip {
          url = "https://github.com/koekeishiya/yabai/releases/download/v7.0.0/yabai-v7.0.0.tar.gz";
          hash = "sha256-FJ4HHiniyvNvwQq5cxpGTAIS8g5vEoHAdtJ33qNzRZo=";
        };
      });
    };
  };
}
