{ pkgs, ... }:
{
  home-manager.users.pwy = {
    home = {
      packages = with pkgs; [
        gnupg
        sequoia
      ];
    };
  };

  programs = {
    gnupg = {
      agent = {
        enable = true;
      };
    };
  };
}
