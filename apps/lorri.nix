{ ... }: {
  home-manager.users.PWY = {
    programs = {
      direnv = {
        enable = true;
        enableZshIntegration = true;
      };
    };
  };

  services = {
    lorri = {
      enable = true;
    };
  };
}
