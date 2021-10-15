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

      kernelModules = [
        "dm-snapshot"
      ];

      postDeviceCommands = ''
        echo '=> Importing pools'
        ${pkgs.zfs}/bin/zpool import rpool

        echo '=> Mounting datasets'
        ${pkgs.zfs}/bin/zfs mount -a
      '';
    };
  };

  fileSystems = {
    "/" = {
      fsType = "zfs";
      device = "rpool/root";
    };

    "/boot" = {
      fsType = "vfat";
      device = "/dev/disk/by-uuid/F23D-9E7F";
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
    { device = "/dev/disk/by-uuid/3ad1150f-77ff-407a-a97c-2342c7582cd6"; }
  ];
}
