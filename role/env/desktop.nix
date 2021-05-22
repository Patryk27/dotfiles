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
      firefox-wayland
      gimp
      google-chrome
      libreoffice
      slack
      vlc
    ];
  };

  fonts = {
    fonts = with pkgs; [
      corefonts
      fira-code
      fira-code-symbols
      roboto
    ];
  };

  hardware = {
    bluetooth = {
      enable = true;

      settings = {
        General = {
          Enable = "Source,Sink,Media,Socket";
        };
      };
    };

    opengl = {
      enable = true;
      driSupport32Bit = true;
    };

    pulseaudio = {
      enable = true;
      extraModules = [ pkgs.pulseaudio-modules-bt ];
      package = pkgs.pulseaudioFull;
    };
  };

  services = {
    hardware = {
      bolt = {
        enable = true;
      };
    };
  };

  sound = {
    enable = true;
  };
}
