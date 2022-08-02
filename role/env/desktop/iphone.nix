{ pkgs, ... }: {
  environment = {
    systemPackages = with pkgs; [
      ifuse
      libimobiledevice
    ];
  };

  services = {
    usbmuxd = {
      enable = true;
    };
  };
}
