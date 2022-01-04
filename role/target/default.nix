{ pkgs, ... }: {
  boot = {
    tmpOnTmpfs = true;
    kernelPackages = pkgs.linuxPackages_latest;

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
      devNodes = "/dev/disk/by-path";
      enableUnstable = true;
    };
  };

  environment = {
    systemPackages = with pkgs; [
      zfs
    ];
  };
}
