{ pkgs, ... }:
{
  networking = {
    hostId = "c7a81ba2";
    hostName = "pfw";

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
