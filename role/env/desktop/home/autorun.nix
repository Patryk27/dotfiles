{ ... }: {
  environment = {
    etc = {
      autorun = {
        source = ./autorun;
      };
    };
  };

  home-manager.users.pwy = { pkgs, ... }: {
    systemd = {
      user = {
        services = {
          autorun = {
            Unit = {
              PartOf = "sway-session.target";
              Requires = "sway-session.target";
              After = "sway-session.target";
            };

            Service = {
              Type = "simple";
              ExecStart = "/etc/autorun/autorun";
              Restart = "no";
            };

            Install = {
              WantedBy = [ "sway-session.target" ];
            };
          };
        };
      };
    };
  };
}
