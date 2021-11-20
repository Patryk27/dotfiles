{ ... }: {
  home-manager.users.pwy = {
    programs = {
      mako = {
        enable = true;
        defaultTimeout = 5000;
        anchor = "bottom-right";

        extraConfig = ''
          [mode=dnd]
          invisible=1
        '';
      };
    };
  };
}
