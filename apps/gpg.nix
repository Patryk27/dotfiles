{ pkgs, ... }:
{
  home-manager.users.pwy = {
    home = {
      packages = with pkgs; [
        gnupg
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
