{ ... }: {
  environment = {
    etc = {
      autorun = {
        source = ./autorun;
      };
    };
  };

  home-manager.users.pwy = {
    systemd = {
      user = {
        services = {
          autorun = {
            Unit = {
              PartOf = [ "graphical-session.target" ];
              After = [ "graphical-session.target" ];
            };

            Service = {
              Type = "simple";
              ExecStart = "/etc/autorun/autorun";
              Restart = "no";
            };

            Install = {
              WantedBy = [ "graphical-session.target" ];
            };
          };
        };
      };
    };
  };
}
