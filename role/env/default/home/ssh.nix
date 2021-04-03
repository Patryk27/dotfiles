{ ... }: {
  home-manager.users.pwy = { ... }: {
    programs = {
      ssh = {
        enable = true;

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

          eric-fort = {
            hostname = "192.168.122.3";
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

          eric-mac = {
            hostname = "192.168.122.6";
            proxyJump = "eric";
            port = 22;
            user = "pwychowaniec";
          };

          eric-public = {
            hostname = "192.168.122.2";
            proxyJump = "eric";
            port = 33000;
            user = "pwy";
          };

          madison = {
            hostname = "10.10.0.3";
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
