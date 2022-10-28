{ config, lib, pkgs, ... }: {
  imports = [
    ../role/env/default.nix
    ../role/target/virtual.nix
  ];

  boot = {
    loader = {
      grub = {
        device = "/dev/vda";
      };
    };
  };

  environment = {
    systemPackages = with pkgs; [
      tmux
    ];
  };

  networking = {
    hostId = "937e9913";
    hostName = "madison";

    firewall = {
      allowedTCPPorts = [
        8443 # LXD
      ];
    };

    interfaces = {
      enp1s0 = {
        useDHCP = true;
      };
    };
  };

  services = {
    logind = {
      extraConfig = ''
        RuntimeDirectorySize = 16G
      '';
    };
  };

  system = {
    stateVersion = "21.11";
  };
}
