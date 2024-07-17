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
      dbeaver-bin
      discord
      ectool
      fd
      ffmpeg-full
      file
      fzf
      gimp
      google-cloud-sdk
      htop
      inkscape
      jq
      just
      killall
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
      watch
    ];
  };

  networking = {
    hostId = "c7a81ba2";
    hostName = "pfw";
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

  time = {
    timeZone = "Europe/Warsaw";
  };
}
