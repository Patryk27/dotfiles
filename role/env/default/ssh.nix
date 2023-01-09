{ ... }: {
  home-manager.users.pwy = {
    programs = {
      ssh = {
        enable = true;
        controlMaster = "auto";
        controlPersist = "10m";

        extraOptionOverrides = {
          "Match Originalhost archive" = ''
            Exec "nmcli | rg Dziupla5"
              Hostname 192.168.1.200
          '';

          "Match Originalhost builder" = ''
            Exec "nmcli | rg Dziupla5"
              Hostname 192.168.1.200
          '';

          "Match Originalhost warp" = ''
            Exec "nmcli | rg Dziupla5"
              Hostname 192.168.1.200
          '';
        };

        matchBlocks = {
          archive = {
            hostname = "10.24.1.2";
            port = 33002;
            user = "pwy";
          };

          builder = {
            hostname = "10.24.1.2";
            port = 33001;
            user = "pwy";
          };

          edge = {
            hostname = "185.238.72.182";
            port = 33000;
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

          mac = {
            hostname = "192.168.122.2";
            port = 22;
            user = "pwy";
          };

          sienna = {
            hostname = "192.168.1.220";
            user = "pi";
          };

          ubu = {
            hostname = "192.168.122.3";
            port = 22;
            user = "pwy";
          };

          warp = {
            hostname = "10.24.1.2";
            port = 33000;
            user = "pwy";
          };

          win = {
            hostname = "192.168.122.4";
            port = 22;
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
