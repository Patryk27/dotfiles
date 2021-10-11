{ pkgs, ... }: {
  imports = [
    ./default.nix
    ./desktop/home.nix
    ./desktop/video.nix
    ./desktop/yubikey.nix
  ];

  environment = {
    systemPackages = with pkgs; [
      audacity
      chromium
      firefox-wayland
      gimp
      libreoffice
      slack
      vlc
    ];
  };

  fonts = {
    fonts =
      let
        iosevka-custom = pkgs.iosevka.override {
          set = "custom";

          privateBuildPlan = {
            family = "Iosevka Custom";
            spacing = "term";
            serifs = "sans";
            no-cv-ss = true;
            no-ligation = true;

            metric-override = {
              leading = 1050;
            };

            widths = {
              normal = {
                menu = 5;
                css = "normal";
                shape = 480;
              };
            };

            variants = {
              inherits = "ss14";
            };
          };
        };

      in
      with pkgs; [
        corefonts
        iosevka-custom
      ];
  };

  hardware = {
    opengl = {
      enable = true;
      driSupport32Bit = true;
    };
  };
}
