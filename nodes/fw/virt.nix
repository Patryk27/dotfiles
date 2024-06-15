{ pkgs, ... }: {
  environment = {
    systemPackages = with pkgs; [
      docker-compose
      virt-manager
    ];
  };

  virtualisation = {
    docker = {
      enable = true;
    };

    libvirtd = {
      enable = true;

      qemu = {
        runAsRoot = true;

        swtpm = {
          enable = true;
        };

        ovmf = {
          enable = true;

          packages = [
            (pkgs.OVMF.override {
              secureBoot = true;
              tpmSupport = true;
            }).fd
          ];
        };
      };
    };
  };
}
