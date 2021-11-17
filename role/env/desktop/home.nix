{ ... }: {
  imports = [
    ./home/autorun.nix
    ./home/emacs.nix
    ./home/keybase.nix
    ./home/kitty.nix
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
