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
      aria
      curl
      fd
      file
      fzf
      github-cli
      google-cloud-sdk
      htop
      jq
      just
      libxml2
      lld
      ncdu
      nethack
      nmap
      postgresql
      python3
      ripgrep
      rsync
      spirv-tools
      sqlite
      unzip
      watch
      wget
      wrk
    ];
  };

  networking = {
    hostId = "c7a81ba2";
    hostName = "pfw";
  };

  time = {
    timeZone = "Europe/Warsaw";
  };
}
