{ ... }: {
  home-manager.users.pwy = {
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
