{ pkgs, ... }: {
  boot = {
    tmpOnTmpfs = true;
    kernelPackages = pkgs.linuxKernel.packages.linux_zen;

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
