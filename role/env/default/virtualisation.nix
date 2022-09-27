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

  virtualisation = {
    docker = {
      enable = true;
      enableNvidia = true;
      liveRestore = false;
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
  };
}
