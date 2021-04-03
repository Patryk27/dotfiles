{ ... }: {
  home-manager.users.pwy = { pkgs, ... }: {
    home = {
      packages = with pkgs; [
        thunderbird
      ];
    };
  };
}
