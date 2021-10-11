{ config, lib, pkgs, ... }: {
  imports = [
    ../role/env/default.nix
    ../role/env/desktop/home/emacs.nix
    ../role/target/virtual.nix
  ];

  boot = {
    loader = {
      grub = {
        device = "/dev/vda";
      };
    };
  };

  environment = {
    systemPackages = with pkgs; [
      tmux
    ];
  };

  networking = {
    hostId = "937e9913";
    hostName = "madison";

    interfaces = {
      enp1s0 = {
        useDHCP = true;
      };
    };
  };

  services = {
    logind = {
      extraConfig = ''
        RuntimeDirectorySize = 16G
      '';
    };
  };
}
