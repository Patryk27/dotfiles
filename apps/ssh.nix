{ ... }: {
  home-manager.users.pwy = {
    programs = {
      ssh = {
        enable = true;
        controlMaster = "auto";
        controlPersist = "10m";

        matchBlocks = {
          archive = {
            # hostname = "10.24.1.2";
            hostname = "192.168.1.200";
            port = 33002;
            user = "pwy";
          };

          gateway = {
            hostname = "142.132.178.21";
            port = 33000;
            user = "pwy";
          };

          glacier = {
            hostname = "zh1258.rsync.net";
            port = 22;
            user = "zh1258";
          };

          sienna = {
            hostname = "192.168.1.220";
            user = "pi";
          };

          ubu = {
            hostname = "192.168.1.113";
            user = "pwy";
          };

          warp = {
            hostname = "10.24.1.2";
            # hostname = "192.168.1.200";
            port = 33000;
            user = "pwy";
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
