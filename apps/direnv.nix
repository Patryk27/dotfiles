{ user, ... }: {
  home-manager.users."${user}" = {
    programs = {
      direnv = {
        enable = true;
      };
    };
  };
}
