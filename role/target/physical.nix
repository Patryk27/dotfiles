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

    bluetooth = {
      enable = true;

      settings = {
        General = {
          Enable = "Source,Sink,Media,Socket";
          ControllerMode = "bredr";
        };
      };
    };

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

    hardware = {
      bolt = {
        enable = true;
      };
    };

    zfs = {
      autoSnapshot = {
        enable = true;
        flags = "-k -p --utc";
        frequent = 12;
        hourly = 24;
        daily = 3;
        monthly = 0;
        weekly = 0;
      };
    };
  };
}
