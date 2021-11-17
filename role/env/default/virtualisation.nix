{ lib, pkgs, ... }: {
  boot = {
    kernelModules = [
      "kvm-intel"
    ];
  };

  environment = {
    systemPackages = with pkgs; [
      docker-compose
    ];
  };

  systemd = {
    services = {
      lxd = {
        serviceConfig = {
          TimeoutStopSec = lib.mkForce "5s";
        };
      };
    };
  };

  virtualisation = {
    docker = {
      enable = true;
    };

    lxc = {
      lxcfs = {
        enable = true;
      };
    };

    lxd = {
      enable = true;
      zfsSupport = true;
    };

    podman = {
      enable = true;
    };
  };
}
