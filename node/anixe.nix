{ pkgs, ... }: {
  imports = [
    ../role/env/desktop.nix
    ../role/target/physical.nix
  ];

  boot = {
    blacklistedKernelModules = [
      "nouveau"
    ];

    initrd = {
      luks = {
        devices = {
          root = {
            device = "/dev/nvme0n1p3";
            preLVM = true;
          };
        };
      };
    };

    kernelModules = [
      "kvm-intel"
    ];
  };

  home-manager.users.pwy = {
    home = {
      packages = with pkgs; [
        (import ../role/app/backup.nix {
          inherit pkgs;
          repo = "pwy@eric-glacier:/home/pwy/backup-anixe";
        })

        openconnect
        vagrant
      ];
    };
  };

  networking = {
    hostId = "c00c043f";
    hostName = "anixe";

    wg-quick = {
      interfaces = {
        wg-fort = {
          address = [ "10.10.0.201/24" ];
          privateKeyFile = "/secrets/wg-fort/private.key";

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
    tlp = {
      enable = true;

      settings = {
        TLP_ENABLE = 1;
        TLP_DEFAULT_MODE = "AC";

        CPU_SCALING_GOVERNOR_ON_AC = "performance";
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

        CPU_SCALING_MIN_FREQ_ON_AC = 2400000;
        CPU_SCALING_MAX_FREQ_ON_AC = 5000000;
        CPU_SCALING_MIN_FREQ_ON_BAT = 800000;
        CPU_SCALING_MAX_FREQ_ON_BAT = 3000000;

        CPU_HWP_ON_AC = "performance";
        CPU_HWP_ON_BAT = "balance_power";

        CPU_BOOST_ON_AC = 1;
        CPU_BOOST_ON_BAT = 0;

        CPU_ENERGY_PERF_POLICY_ON_AC = "performance";
        CPU_ENERGY_PERF_POLICY_ON_BAT = "balance_power";

        SCHED_POWERSAVE_ON_AC = 0;
        SCHED_POWERSAVE_ON_BAT = 1;

        RUNTIME_PM_DRIVER_BLACKLIST = "nouveau nvidia";

        START_CHARGE_THRESH_BAT0 = 90;
        STOP_CHARGE_THRESH_BAT0 = 100;
      };
    };
  };

  sops = {
    secrets = {
      backup-passphrase = {
        key = "backup-passphrase--anixe";
      };
    };
  };

  virtualisation = {
    docker = {
      storageDriver = "zfs";
    };

    # TODO (seem not to work at the moment)
    # virtualbox = {
    #   host = {
    #     enable = true;
    #     headless = true;
    #   };
    # };
  };
}
