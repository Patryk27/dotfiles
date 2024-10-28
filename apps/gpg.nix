{ pkgs, user, ... }:
{
  home-manager.users."${user}" = {
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
