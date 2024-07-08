{ pkgs, ... }: {
  home-manager.users.pwy = {
    home = {
      packages = with pkgs; [
        nodePackages.typescript
        nodePackages.typescript-language-server
        vue-language-server
      ];
    };
  };
}
