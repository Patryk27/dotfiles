{ pkgs, ... }: {
  environment = {
    systemPackages = with pkgs; [
      aria
      bmon
      curl
      nmap
      rsync
      snx-rs
      wget
      wrk
    ];
  };

  networking = {
    wireguard = {
      interfaces = {
        wg-fort = {
          ips = [ "10.24.1.10/24" ];
          listenPort = 51820;
          privateKeyFile = "/var/secrets/wg-fort/private.key";

          peers = [
            # gateway
            {
              publicKey = "GwhWP0DClVw9fY7PJidPuZfOzBhxhcnjTnO+8i1Z50w=";
              allowedIPs = [ "10.24.1.0/24" ];
              endpoint = "142.132.178.21:51820";
              persistentKeepalive = 10;
            }
          ];
        };
      };
    };
  };
}
