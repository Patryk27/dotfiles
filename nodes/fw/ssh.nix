{ pkgs, ... }:
let
  if-outside-home = "! ${pkgs.iw}/bin/iw dev wlp1s0 info | grep -q Dziupla";

  matchBlocks = lib: {
    archive = {
      hostname = "192.168.1.200";
      port = 33002;
      user = "pwy";
    };

    archive--wg = lib.hm.dag.entryBefore [ "archive" ] {
      match = ''Host archive Exec "${if-outside-home}"'';
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
      hostname = "192.168.122.227";
      user = "pwy";
    };

    ubu-arm64 = {
      hostname = "192.168.122.62";
      user = "pwy";
    };

    warp = {
      hostname = "192.168.1.200";
      port = 33000;
      user = "pwy";
    };

    warp--wg = lib.hm.dag.entryBefore [ "warp" ] {
      match = ''Host warp Exec "${if-outside-home}"'';
      hostname = "10.24.1.2";
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
              archive--wg
              gateway
              warp
              warp--wg
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
