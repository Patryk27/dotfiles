{ pkgs, ... }: {
  boot = {
    tmpOnTmpfs = true;
    kernelPackages = pkgs.linuxPackages_latest;

    kernelParams = [
      "mitigations=off"
    ];

    kernel = {
      sysctl = {
        "fs.inotify.max_user_watches" = "1048576";
      };
    };

    loader = {
      efi = {
        canTouchEfiVariables = true;
      };
    };
  };
}
