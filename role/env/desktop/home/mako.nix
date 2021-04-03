{ ... }: {
  home-manager.users.pwy = {
    programs = {
      mako = {
        enable = true;
        defaultTimeout = 5000;
      };
    };
  };
}
