{ pkgs, ... }:
{
  imports = [
    ../apps/avr.nix
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
      p7zip
      postgresql
      python3
      ripgrep
      slack
      sourcekit-lsp
      sqlite
      unrar
      unzip
      watch
      zip
      zstd
    ];
  };
}
