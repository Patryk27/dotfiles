{ ... }: {
  home-manager.users.pwy = {
    programs = {
      ssh = {
        enable = true;
        controlMaster = "yes";
        controlPersist = "10m";

        matchBlocks = {
          eric = {
            hostname = "188.122.2.73";
            port = 33000;
            user = "pwy";
          };

          eric-builder = {
            hostname = "192.168.122.7";
            proxyJump = "eric";
            port = 33000;
            user = "pwy";
          };

          eric-glacier = {
            hostname = "192.168.122.5";
            proxyJump = "eric";
            port = 33000;
            user = "pwy";
          };

          eric-home = {
            hostname = "192.168.1.201";
            port = 33000;
            user = "pwy";
          };

          eric-nas = {
            hostname = "192.168.122.8";
            proxyJump = "eric";
            port = 33000;
            user = "pwy";
          };

          eric-public = {
            hostname = "192.168.122.2";
            proxyJump = "eric";
            port = 33000;
            user = "pwy";
          };

          mac = {
            hostname = "10.10.0.4";
            port = 22;
            user = "pwychowaniec";
          };

          madison = {
            hostname = "188.122.2.73";
            port = 33002;
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
