{ pkgs, ... }: {
  home-manager.users.pwy = {
    home = {
      packages = with pkgs; [
        php80
        php80Packages.composer
      ];
    };
  };
}
