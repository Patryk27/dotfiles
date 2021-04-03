{ pkgs, ... }: {
  environment = {
    systemPackages = with pkgs; [
      yubikey-manager
      yubikey-personalization
    ];
  };

  services = {
    udev = {
      packages = with pkgs; [
        yubikey-personalization
      ];
    };
  };
}
