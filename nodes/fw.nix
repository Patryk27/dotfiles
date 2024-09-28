{ pkgs, ... }: {
  imports = [
    ../apps/cpp.nix
    ../apps/csharp.nix
    ../apps/dbg.nix
    ../apps/direnv.nix
    ../apps/emacs.nix
    ../apps/git.nix
    ../apps/gpg.nix
    ../apps/js.nix
    ../apps/kitty.nix
    ../apps/nix.nix
    ../apps/res.nix
    ../apps/rust.nix
    ../apps/ssh.nix
    ../apps/vim.nix
    ../apps/zsh.nix

    ./fw/gui.nix
    ./fw/hw.nix
    ./fw/iphone.nix
    ./fw/net.nix
    ./fw/schedules.nix
    ./fw/ssh.nix
    ./fw/user.nix
    ./fw/virt.nix
  ];

  # TODO remove extra
  environment = {
    systemPackages = with pkgs; [
      anki-bin
      asciinema
      audacity
      borgbackup
      bzip2
      chromium
      colmap
      dbeaver-bin
      discord
      ffmpeg-full
      file
      firefox
      fzf
      gimp
      google-cloud-sdk
      htop
      inkscape
      jq
      just
      kdenlive
      killall
      libreoffice-qt
      libxml2
      lld
      lm_sensors
      moonlight-qt
      ncdu
      nethack
      opensplat
      postgresql
      powertop
      python3
      ripgrep
      slack
      spirv-tools
      spotify
      sqlite
      unnethack
      unzip
      vlc
      watch
      zip
    ];
  };

  home-manager.users.root = {
    programs = {
      ssh = {
        enable = true;

        matchBlocks = {
          archive = {
            port = 33002;
            user = "pwy";
            hostname = "10.24.1.2";
            proxyJump = "gateway";
          };

          gateway = {
            hostname = "142.132.178.21";
            port = 33000;
            user = "pwy";
          };
        };
      };
    };
  };

  networking = {
    hostId = "c7a81ba2";
    hostName = "pfw";
  };

  nixpkgs = {
    config = {
      permittedInsecurePackages = [
        "freeimage-unstable-2021-11-01"
      ];
    };
  };

  programs = {
    steam = {
      enable = true;
    };
  };

  services = {
    fwupd = {
      enable = true;
    };
  };

  systemd = {
    services = {
      disable-led = {
        script = ''
          ${pkgs.fw-ectool}/bin/ectool led power off
        '';

        wantedBy = [ "multi-user.target" ];
      };
    };

    timers = {
      fwupd-refresh = {
        enable = false;
      };
    };
  };

  time = {
    timeZone = "Europe/Warsaw";
  };
}
