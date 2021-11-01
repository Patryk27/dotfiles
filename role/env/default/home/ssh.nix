{ ... }: {
  home-manager.users.pwy = {
    programs = {
      ssh = {
        enable = true;
        controlMaster = "auto";
        controlPersist = "10m";

        matchBlocks = {
          eric = {
            hostname = "188.122.2.73";
            port = 33000;
            user = "pwy";
          };

          eric-archive = {
            hostname = "188.122.2.73";
            port = 33002;
            user = "pwy";
          };

          eric-builder = {
            hostname = "188.122.2.73";
            port = 33001;
            user = "pwy";
          };

          eric-home = {
            hostname = "192.168.1.201";
            port = 33000;
            user = "pwy";
          };

          eric-public = {
            hostname = "192.168.122.2";
            proxyJump = "eric";
            port = 33000;
            user = "pwy";
          };

          glacier = {
            hostname = "zh1258.rsync.net";
            port = 22;
            user = "zh1258";
          };

          mac = {
            hostname = "10.10.0.4";
            port = 22;
            user = "pwychowaniec";
          };

          madison = {
            hostname = "188.122.2.73";
            port = 33003;
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
