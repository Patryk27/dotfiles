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
            # hostname = "10.24.1.2";
            hostname = "192.168.1.200";
            port = 33000;
            user = "pwy";
          };

          # --- #

          "gitlab.pwy.io" = {
            port = 47000;
            user = "git";
          };

          # --- #

          anx-wrop-aclr8-p1 = {
            hostname = "192.168.194.232";
            user = "patryk.wychowaniec";
          };

          anx-wrop-aclr8-p2 = {
            hostname = "192.168.194.132";
            user = "patryk.wychowaniec";
          };

          anx-wrop-aclr8-p3 = {
            hostname = "192.168.194.137";
            user = "patryk.wychowaniec";
          };

          anx-gcpp-mercures-mpg-v1 = {
            hostname = "34.76.150.210";
            user = "patryk.wychowaniec";
          };

          anx-gcpp-mercures-mpg-v2 = {
            hostname = "34.76.252.23";
            user = "patryk.wychowaniec";
          };

          anx-gcpp-mercures-mpg-v3 = {
            hostname = "35.233.81.152";
            user = "patryk.wychowaniec";
          };

          anx-gcpp-mercures-dc-v1 = {
            hostname = "35.205.67.162";
            user = "patryk.wychowaniec";
          };

          anx-gcpp-hx-v1 = {
            hostname = "35.189.219.168";
            user = "patryk.wychowaniec";
          };

          gcpa-hx-v1 = {
            hostname = "34.107.119.213";
            user = "patryk.wychowaniec";
          };
        };

        extraConfig = ''
          PubkeyAcceptedKeyTypes +ssh-rsa
        '';
      };
    };
  };
}
