{ pkgs, ... }: {
  imports = [
    ./default.nix
  ];

  boot = {
    loader = {
      grub = {
        copyKernels = true;
      };

      systemd-boot = {
        enable = true;
      };
    };
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
        frequent = 8;
        monthly = 1;
      };
    };
  };
}
