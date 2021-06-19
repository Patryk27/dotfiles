{ config, ... }: {
  home-manager.users.pwy = {
    programs = {
      taskwarrior = {
        enable = true;

        config = {
          recurrence = if config.networking.hostName == "lenovo" then "on" else "off";

          data = {
            location = "~/.task";
          };
        };

        extraConfig = ''
          include ${config.sops.secrets.taskrc.path}
        '';
      };
    };
  };
}
