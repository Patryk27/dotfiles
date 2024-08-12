# TODO swap

{ pkgs, ... }: {
  imports = [
    ../apps/cpp.nix
    ../apps/csharp.nix
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
    ./fw/net.nix
    ./fw/schedules.nix
    ./fw/user.nix
    ./fw/virt.nix
  ];

  environment = {
    systemPackages = with pkgs; [
      anki-bin
      audacity
      borgbackup
      bzip2
      chromium
      dbeaver-bin
      dig
      discord
      ectool
      fd
      ffmpeg-full
      file
      firefox
      fzf
      gdb
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
      linuxPackages.perf
      lld
      lm_sensors
      moonlight-qt
      ncdu
      nethack
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
      valgrind
      vlc
      watch
      zip
    ];
  };

  networking = {
    hostId = "c7a81ba2";
    hostName = "pfw";

    networkmanager = {
      dns = "systemd-resolved";
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

    resolved = {
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
