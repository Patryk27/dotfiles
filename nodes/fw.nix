# TODO swap

{ pkgs, ... }: {
  imports = [
    ../apps/cpp.nix
    ../apps/csharp.nix
    ../apps/direnv.nix
    ../apps/emacs.nix
    ../apps/git.nix
    ../apps/gpg.nix
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
      discord
      ectool
      fd
      ffmpeg-full
      file
      fzf
      google-cloud-sdk
      htop
      jq
      just
      killall
      libxml2
      lld
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

  programs = {
    steam = {
      enable = true;
    };
  };

  networking = {
    hostId = "c7a81ba2";
    hostName = "pfw";
  };

  time = {
    timeZone = "Europe/Warsaw";
  };
}
