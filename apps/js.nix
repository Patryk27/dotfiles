{ pkgs, user, ... }:
{
  home-manager.users."${user}" = {
    home = {
      packages = with pkgs; [
        nodePackages.prettier
        nodePackages.typescript
        nodePackages.typescript-language-server
        nodejs
        vue-language-server
      ];
    };
  };
}
