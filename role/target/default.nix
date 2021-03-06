{ pkgs, ... }: {
  boot = {
    tmpOnTmpfs = true;
    kernelPackages = pkgs.linuxPackages_5_17; # TODO upgrade after ZFS catches up

    kernelParams = [
      "mitigations=off"
    ];

    kernel = {
      sysctl = {
        "fs.inotify.max_user_watches" = 1048576;
        "vm.swappiness" = 1;
      };
    };

    loader = {
      efi = {
        canTouchEfiVariables = true;
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
}
