{ pkgs, ... }: {
  imports = [
    ./default.nix
    ./desktop/home.nix
    ./desktop/iphone.nix
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
              leading = 1025;
            };

            widths = {
              normal = {
                menu = 5;
                css = "normal";
                shape = 480;
              };
            };

            variants = {
              inherits = "ss08";
            };
          };
        };

      in
      with pkgs; [
        corefonts
        iosevka-custom
        noto-fonts-emoji
      ];
  };

  hardware = {
    opengl = {
      enable = true;
      driSupport32Bit = true;
    };
  };

  nixpkgs = {
    config = {
      chromium = {
        enableWideVine = true;
      };
    };
  };
}
