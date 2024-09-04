{ pkgs, ... }: {
  environment = {
    systemPackages = with pkgs; [
      aria
      bmon
      curl
      dig
      nmap
      rsync
      wget
      wrk
    ];
  };

  networking = {
    networkmanager = {
      dns = "systemd-resolved";
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
