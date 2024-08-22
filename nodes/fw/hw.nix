{ config, ... }: {
  boot = {
    kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
    supportedFilesystems = [ "zfs" ];

    kernelParams = [
      "mitigations=off"
    ];

    initrd = {
      availableKernelModules = [
        "nvme"
        "xhci_pci"
        "thunderbolt"
        "usb_storage"
        "usbhid"
        "sd_mod"
      ];

      kernelModules = [
        "kvm-amd"
      ];

      luks = {
        devices = {
          root = {
            device = "/dev/nvme0n1p2";
            preLVM = true;
          };
        };
      };
    };

    loader = {
      grub = {
        enable = true;
        zfsSupport = true;
        efiSupport = true;
        efiInstallAsRemovable = true;
        enableCryptodisk = true;

        mirroredBoots = [
          {
            devices = [ "nodev" ];
            path = "/boot";
          }
        ];
      };
    };

    zfs = {
      devNodes = "/dev/mapper";
    };
  };

  fileSystems = {
    "/" = {
      device = "rpool";
      fsType = "zfs";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/3F33-58F9";
      fsType = "vfat";
      options = [ "fmask=0022" "dmask=0022" ];
    };

    "/nix" = {
      device = "rpool/nix";
      fsType = "zfs";
    };
  };

  hardware = {
    enableRedistributableFirmware = true;

    graphics = {
      enable = true;
    };

    cpu = {
      amd = {
        updateMicrocode = true;
      };
    };
  };

  services = {
    fprintd = {
      enable = true;
    };

    zfs = {
      autoScrub = {
        enable = true;
        interval = "Mon *-*-* 12:00:00";
      };

      autoSnapshot = {
        enable = true;
        flags = "-k -p --utc";
        monthly = 0;
      };
    };
  };

  swapDevices = [
    { device = "/dev/nvme0n1p3"; }
  ];
}
