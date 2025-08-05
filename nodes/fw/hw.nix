{ pkgs, ... }:
{
  boot = {
    supportedFilesystems = [ "zfs" ];
    kernelPackages = pkgs.linuxPackages_6_15;

    kernelParams = [
      "amdgpu.dcdebugmask=0x410"
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
      fsType = "zfs";
      device = "rpool";
    };

    "/boot" = {
      fsType = "vfat";
      device = "/dev/disk/by-uuid/3F33-58F9";

      options = [
        "fmask=0022"
        "dmask=0022"
      ];
    };

    "/nix" = {
      fsType = "zfs";
      device = "rpool/nix";
    };

    # Secrets need to be mounted explicitly, otherwise zfs auto-imports them too
    # late for agenix to pick up
    "/secrets" = {
      fsType = "zfs";
      device = "rpool/secrets";
      neededForBoot = true;
    };
  };

  hardware = {
    enableRedistributableFirmware = true;

    graphics = {
      enable = true;

      extraPackages = with pkgs; [
        rocmPackages.clr.icd
      ];
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
      };
    };
  };

  swapDevices = [
    { device = "/dev/nvme0n1p3"; }
  ];
}
