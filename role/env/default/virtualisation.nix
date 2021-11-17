{ pkgs, ... }: {
  boot = {
    kernelModules = [
      "kvm-intel"
    ];
  };

  environment = {
    systemPackages = with pkgs; [
      ctop
      docker-compose
    ];
  };

  virtualisation = {
    docker = {
      enable = true;
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
