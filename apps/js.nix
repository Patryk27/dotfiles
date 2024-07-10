{ pkgs, ... }: {
  home-manager.users.pwy = {
    home = {
      packages = with pkgs; [
        nodePackages.typescript
        nodePackages.typescript-language-server
        nodejs
        vue-language-server
      ];
    };
  };
}
