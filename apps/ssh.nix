{ pkgs, ... }:
let
  wifi-check = "${pkgs.iw}/bin/iw dev wlp1s0 info | grep -q Desafinado";

in
{
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
            user = "pwy";
          };

          ubu--local = {
            match = ''OriginalHost ubu Exec "${wifi-check}"'';
            hostname = "192.168.1.113";
          };

          ubu--wg = lib.hm.dag.entryAfter [ "ubu--local" ] {
            match = "OriginalHost ubu";
            hostname = "10.24.1.3";
          };

          warp = {
            port = 33000;
            user = "pwy";
          };

          warp--local = {
            match = ''OriginalHost warp Exec "${wifi-check}"'';
            hostname = "192.168.1.200";
          };

          warp--wg = lib.hm.dag.entryAfter [ "warp--local" ] {
            match = "OriginalHost warp";
            hostname = "10.24.1.2";
          };

          win = {
            user = "Komputer";
            hostname = "192.168.1.114";
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

          anx-gcpa-hx-v1 = {
            hostname = "34.107.119.213";
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

          anx-gcpp-mercures-mpg-v4 = {
            hostname = "34.76.100.205";
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

          anx-yta-5 = {
            hostname = "10.117.106.5";
            user = "patryk.wychowaniec";
          };

          anx-yta-7 = {
            hostname = "10.117.106.7";
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
