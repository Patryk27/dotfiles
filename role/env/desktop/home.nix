{ ... }: {
  imports = [
    ./home/alacritty.nix
    ./home/autorun.nix
    ./home/emacs.nix
    ./home/kanshi.nix
    ./home/keybase.nix
    ./home/mako.nix
    ./home/redshift.nix
    ./home/sway.nix
    ./home/thunderbird.nix
    ./home/waybar.nix
    ./home/wofi.nix
  ];

  home-manager.users.pwy = {
    fonts = {
      fontconfig = {
        enable = true;
      };
    };
  };
}
