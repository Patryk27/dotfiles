{ pkgs, ... }:
let
  if-outside-home = "! ${pkgs.iw}/bin/iw dev wlp1s0 info | grep -q Dziupla";

  matchBlocks = lib: {
    archive = {
      hostname = "192.168.1.200";
      port = 33002;
      user = "pwy";
    };

    archive--ext = lib.hm.dag.entryAfter [ "archive" ] {
      match = ''OriginalHost archive Exec "${if-outside-home}"'';
      hostname = "10.24.1.2";
      proxyJump = "gateway";
      forwardAgent = true;
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
      hostname = "192.168.122.227";
      user = "pwy";
    };

    warp = {
      hostname = "192.168.1.200";
      port = 33000;
      user = "pwy";
    };

    warp--ext = lib.hm.dag.entryAfter [ "warp" ] {
      match = ''OriginalHost warp Exec "${if-outside-home}"'';
      hostname = "10.24.1.2";
      proxyJump = "gateway";
      forwardAgent = true;
    };

    warp-ubu = {
      hostname = "192.168.122.92";
      user = "pwy";
      proxyJump = "warp";
    };
  };

in
{
  home-manager.users.root =
    { lib, ... }:
    {
      programs = {
        ssh = {
          enable = true;

          matchBlocks = {
            inherit (matchBlocks lib)
              archive
              archive--ext
              gateway
              warp
              warp--ext
              ;
          };
        };
      };
    };

  home-manager.users.pwy =
    { lib, ... }:
    {
      programs = {
        ssh = {
          matchBlocks = matchBlocks lib;
        };
      };
    };
}
