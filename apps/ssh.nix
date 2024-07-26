{ pkgs, ... }:
let
  wifi-check = "${pkgs.iw}/bin/iw dev wlp1s0 info | grep -q Dziupla";

in
{
  home-manager.users.root = {
    programs = {
      ssh = {
        enable = true;

        matchBlocks = {
          archive = {
            port = 33002;
            user = "pwy";
            hostname = "10.24.1.2";
          };
        };
      };
    };
  };

  home-manager.users.pwy = { lib, ... }: {
    home = {
      packages = with pkgs; [
        sshfs
      ];
    };

    programs = {
      ssh = {
        enable = true;
        controlMaster = "auto";
        controlPersist = "10m";
        serverAliveInterval = 30;

        matchBlocks = {
          archive = {
            port = 33002;
            user = "pwy";
          };

          archive--local = {
            match = ''OriginalHost archive Exec "${wifi-check}"'';
            hostname = "192.168.1.200";
          };

          archive--wg = lib.hm.dag.entryAfter [ "archive--local" ] {
            match = "OriginalHost archive";
            hostname = "10.24.1.2";
          };

          gateway = {
            hostname = "142.132.178.21";
            port = 33000;
            user = "pwy";
          };

          glacier = {
            hostname = "u393702.your-storagebox.de";
            port = 23;
            user = "u393702";
          };

          sienna = {
            hostname = "192.168.1.220";
            user = "pi";
          };

          ubu = {
            hostname = "192.168.1.113";
            user = "pwy";
          };

          ubu-wg = {
            hostname = "10.24.1.3";
            user = "pwy";
          };

          warp = {
            hostname = "192.168.1.200";
            port = 33000;
            user = "pwy";
          };

          warp-wg = {
            hostname = "10.24.1.2";
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

          anx-gcpa = {
            hostname = "34.107.119.213";
            user = "patryk.wychowaniec";
          };

          anx-gcpp = {
            hostname = "34.76.100.205";
            user = "patryk.wychowaniec";
          };

          anx-lv = {
            hostname = "10.33.25.12";
            user = "patryk.wychowaniec";
          };

          anx-pozpdariv1 = {
            hostname = "pozpdariv1.hrc.lan";
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
