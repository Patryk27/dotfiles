{ pkgs, ... }: {
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
