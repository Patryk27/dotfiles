{ modulesPath, pkgs, ... }: {
  imports = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot = {
    kernelModules = [
      "kvm-intel"
    ];

    kernelParams = [
      "boot.shell_on_fail"
    ];

    initrd = {
      availableKernelModules = [
        "xhci_pci"
        "nvme"
        "usb_storage"
        "sd_mod"
      ];
    };
  };

  fileSystems = {
    "/" = {
      device = "rpool/root";
      fsType = "zfs";
    };

    "/home" = {
      device = "rpool/home";
      fsType = "zfs";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/8350-BA40";
      fsType = "vfat";
    };
  };

  hardware = {
    video = {
      hidpi = {
        enable = true;
      };
    };
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/d9b96e55-e1e6-4649-8e92-2fa62d413a25"; }
  ];
}
