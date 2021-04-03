{ ... }: {
  home-manager.users.pwy = { pkgs, ... }: {
    programs = {
      mako = {
        enable = true;
        defaultTimeout = 5000;
      };
    };
  };
}
