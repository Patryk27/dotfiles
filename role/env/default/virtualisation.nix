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
      liveRestore = false;
    };

    lxc = {
      lxcfs = {
        enable = true;
      };
    };

    # TODO https://github.com/NixOS/nixpkgs/issues/183759
    # lxd = {
    #   enable = true;
    #   zfsSupport = true;
    # };

    podman = {
      enable = true;
    };
  };
}
