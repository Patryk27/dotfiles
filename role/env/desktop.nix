{ pkgs, ... }: {
  imports = [
    ./default.nix
    ./desktop/emacs.nix
    ./desktop/i3.nix
    ./desktop/iphone.nix
    ./desktop/kitty.nix
    ./desktop/polybar.nix
    ./desktop/rofi.nix
  ];

  environment = {
    pathsToLink = [
      "/libexec"
    ];

    systemPackages = with pkgs; [
      audacity
      chromium
      firefox
      gimp
      libreoffice
      okular
      simplescreenrecorder
      slack
      spotify
      thunderbird
      vlc
      xclip
      xorg.xmodmap
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
        iosevka-custom
        noto-fonts-emoji
      ];
  };

  hardware = {
    opengl = {
      enable = true;
      driSupport32Bit = true;
    };

    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
    };
  };

  home-manager.users.pwy = {
    home = {
      file = {
        ".wallpaper" = {
          source = ./desktop/wallpaper.png;
        };
      };
    };
  };

  nixpkgs = {
    config = {
      pulseaudio = true;

      chromium = {
        enableWideVine = true;
      };
    };
  };

  programs = {
    dconf = {
      enable = true;
    };

    file-roller = {
      enable = true;
    };

    thunar = {
      plugins = with pkgs.xfce; [
        thunar-archive-plugin
      ];
    };
  };

  services = {
    xserver = {
      enable = true;
      layout = "pl";
      autoRepeatDelay = 200;
      autoRepeatInterval = 40;

      libinput = {
        enable = true;
      };

      displayManager = {
        autoLogin = {
          enable = true;
          user = "pwy";
        };
      };

      desktopManager = {
        session = [
          { name = "default"; start = ""; }
        ];

        xfce = {
          enable = true;
          noDesktop = true;
          enableXfwm = false;
        };

        xterm = {
          enable = false;
        };
      };
    };
  };
}
