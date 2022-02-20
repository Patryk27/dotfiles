{ ... }: {
  imports = [
    ./home/anki.nix
    ./home/autorun.nix
    ./home/emacs.nix
    ./home/gammastep.nix
    ./home/keybase.nix
    ./home/kitty.nix
    ./home/mako.nix
    ./home/slack.nix
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
