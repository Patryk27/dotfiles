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
    ./fw/user.nix
    ./fw/virt.nix
  ];

  environment = {
    systemPackages = with pkgs; [
      anki-bin
      aria
      audacity
      curl
      fd
      ffmpeg-full
      file
      fzf
      github-cli
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
      nmap
      postgresql
      powertop
      python3
      ripgrep
      rsync
      slack
      spirv-tools
      spotify
      sqlite
      unnethack
      unzip
      valgrind
      watch
      wget
      wrk
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
