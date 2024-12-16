{ pkgs, ... }:
{
  imports = [
    ../apps/cpp.nix
    ../apps/dbg.nix
    ../apps/direnv.nix
    ../apps/emacs.nix
    ../apps/git.nix
    ../apps/gpg.nix
    ../apps/js.nix
    ../apps/kitty.nix
    ../apps/net.nix
    ../apps/nix.nix
    ../apps/rust.nix
    ../apps/ssh.nix
    ../apps/vim.nix
    ../apps/zsh.nix
  ];

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
  };

  environment = {
    systemPackages = with pkgs; [
      asciinema
      audacity
      bzip2
      ffmpeg-full
      file
      fzf
      gimp
      glibtool
      gocryptfs
      htop
      inetutils
      jq
      just
      killall
      libxml2
      lld
      magic-wormhole
      ncdu
      postgresql
      python3
      ripgrep
      sqlite
      unzip
      watch
      zip
      zstd
    ];
  };
}
