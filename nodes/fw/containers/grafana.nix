{
  autoStart = true;
  privateNetwork = true;
  hostAddress = "10.254.1.1";
  localAddress = "10.254.1.2";

  config =
    { pkgs, ... }:
    {
      networking = {
        firewall = {
          enable = false;
        };
      };

      services = {
        grafana = {
          enable = true;

          settings = {
            server = {
              domain = "localhost";
              http_addr = "0.0.0.0";
              http_port = 80;
              protocol = "http";
            };
          };
        };

        prometheus = {
          enable = true;
          retentionTime = "10y";

          scrapeConfigs = [
            {
              job_name = "fw";

              static_configs = [
                { targets = [ "10.254.1.1:9100" ]; } # prometheus.exporters.node
                { targets = [ "10.254.1.1:9134" ]; } # prometheus.exporters.zfs
              ];
            }
          ];
        };
      };

      system = {
        stateVersion = "25.11";
      };
    };
}
