{ pkgs, ... }: {
  imports = [
    ./default.nix
  ];

  boot = {
    kernel = {
      sysctl = {
        "vm.swappiness" = 1;
      };
    };

    loader = {
      grub = {
        copyKernels = true;
      };

      systemd-boot = {
        enable = true;
      };
    };

    supportedFilesystems = [
      "zfs"
    ];

    zfs = {
      enableUnstable = true;
    };
  };

  environment = {
    systemPackages = with pkgs; [
      zfs
    ];
  };

  hardware = {
    enableAllFirmware = true;

    cpu = {
      intel = {
        updateMicrocode = true;
      };
    };
  };

  powerManagement = {
    powertop = {
      enable = true;
    };
  };

  services = {
    fwupd = {
      enable = true;
    };

    zfs = {
      autoSnapshot = {
        enable = true;
        flags = "-k -p --utc";
      };
    };
  };
}
