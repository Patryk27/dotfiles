{ ... }: {
  home-manager.users.pwy = {
    programs = {
      ssh = {
        enable = true;
        controlMaster = "auto";
        controlPersist = "10m";

        matchBlocks = {
          edge = {
            hostname = "185.238.72.182";
            port = 33000;
            user = "pwy";
          };

          eric = {
            hostname = "10.24.1.2";
            port = 33000;
            user = "pwy";
          };

          eric-archive = {
            hostname = "10.24.1.2";
            port = 33002;
            user = "pwy";
          };

          eric-builder = {
            hostname = "10.24.1.2";
            port = 33001;
            user = "pwy";
          };

          glacier = {
            hostname = "zh1258.rsync.net";
            port = 22;
            user = "zh1258";
          };

          mac = {
            proxyJump = "eric";
            hostname = "192.168.122.15";
            port = 22;
            user = "pwychowaniec";
          };

          madison = {
            proxyJump = "eric";
            hostname = "192.168.122.4";
            port = 33000;
            user = "pwy";

            remoteForwards = [
              {
                bind.address = "/run/user/1000/gnupg/S.gpg-agent";
                host.address = "/run/user/1000/gnupg/S.gpg-agent.extra";
              }
              {
                bind.address = "/run/user/1000/gnupg/S.gpg-agent.ssh";
                host.address = "/run/user/1000/gnupg/S.gpg-agent.ssh";
              }
            ];
          };

          sienna = {
            hostname = "192.168.1.220";
            user = "pi";
          };

          # --- #

          "gitlab.pwy.io" = {
            port = 47000;
            user = "git";
          };
        };
      };
    };
  };
}
