{ pkgs, ... }:
{
  networking = {
    hostId = "c7a81ba2";
    hostName = "pfw";

    extraHosts = ''
      10.24.1.2 archive.lan
      10.254.1.2 grafana.local
    '';

    networkmanager = {
      dns = "systemd-resolved";
    };

    wireguard = {
      interfaces = {
        wg-fort = {
          ips = [ "10.24.1.10/24" ];
          privateKeyFile = "/secrets/wg-fort/private.key";

          peers = [
            # gateway
            {
              publicKey = "GwhWP0DClVw9fY7PJidPuZfOzBhxhcnjTnO+8i1Z50w=";
              allowedIPs = [ "10.24.1.0/24" ];
              endpoint = "142.132.178.21:53";
              persistentKeepalive = 10;
            }
          ];
        };
      };
    };
  };

  services = {
    mullvad-vpn = {
      enable = true;
      package = pkgs.mullvad-vpn;
    };

    resolved = {
      enable = true;
    };
  };
}
