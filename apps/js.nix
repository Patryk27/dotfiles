{ pkgs, ... }: {
  home-manager.users.pwy = {
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
