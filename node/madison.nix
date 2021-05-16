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
      alacritty
      tmux
    ];
  };

  home-manager.users.pwy = {
    home = {
      sessionVariables = {
        SSH_AUTH_SOCK = "/run/user/1000/gnupg/S.gpg-agent.ssh";
      };
    };

    services = {
      gpg-agent = {
        enable = lib.mkForce false;
      };
    };
  };

  networking = {
    hostId = "937e9913";
    hostName = "madison";

    interfaces = {
      enp1s0 = {
        useDHCP = true;
      };
    };

    wg-quick = {
      interfaces = {
        wg-fort = {
          address = [ "10.10.0.3/24" ];
          privateKeyFile = "/secrets/wg-fort/private.key";

          postUp = ''
            /run/wrappers/bin/ping -c60 10.10.0.1
          '';

          peers = [
            {
              publicKey = "YqaSjTecpYwlYUX2Y6kUbNq4pV34QCr4GbOqMDMS0T0=";
              allowedIPs = [ "10.10.0.0/24" ];
              endpoint = "188.122.2.73:59999";
              persistentKeepalive = 25;
            }
          ];
        };
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
