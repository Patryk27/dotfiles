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
