{ config, pkgs, ... }: {
  imports = [
    ../role/env/desktop.nix
    ../role/target/physical.nix
  ];

  boot = {
    blacklistedKernelModules = [
      "nouveau"
    ];

    extraModulePackages = with config.boot.kernelPackages; [
      acpi_call
    ];

    extraModprobeConfig = "options snd_hda_intel power_save=1";

    kernelModules = [
      "acpi_call"
    ];

    kernelParams = [
      "i915.enable_fbc=1"
      "i915.enable_gvt=1"
      "psmouse.synaptics_intertouch=1"
      "thinkpad_acpi.fan_control=1"
    ];

    initrd = {
      availableKernelModules = [
        "battery"
      ];

      luks = {
        devices = {
          root = {
            device = "/dev/disk/by-uuid/47ebd8a5-bf81-46ce-b572-c8fc53e1a80c";
            preLVM = true;
          };
        };
      };
    };
  };

  console = {
    font = "latarcyrheb-sun32";
  };

  environment = {
    systemPackages = with pkgs; [
      mullvad-vpn
    ];
  };

  fonts = {
    fontconfig = {
      dpi = 160;
    };
  };

  home-manager.users.pwy = {
    home = {
      packages = with pkgs; [
        (import ../role/app/backup.nix {
          inherit pkgs;
          repo = "pwy@eric-glacier:/home/pwy/backup-lenovo";
        })

        anki
        darktable
        geeqie
        obs-studio
        rawtherapee
        signal-desktop
        virt-manager
      ];

      sessionVariables = {
        MESA_LOADER_DRIVER_OVERRIDE = "iris";
      };
    };
  };

  networking = {
    hostId = "8be46994";
    hostName = "lenovo";

    firewall = {
      allowedTCPPorts = [
        1313 # Hugo
      ];
    };

    wg-quick = {
      interfaces = {
        wg-fort = {
          address = [ "10.10.0.200/24" ];
          privateKeyFile = "/secrets/wg-fort/private.key";

          peers = [
            {
              endpoint = "188.122.2.73:59999";
              publicKey = "YqaSjTecpYwlYUX2Y6kUbNq4pV34QCr4GbOqMDMS0T0=";
              allowedIPs = [ "10.10.0.0/24" ];
              persistentKeepalive = 25;
            }
          ];
        };
      };
    };
  };

  nixpkgs = {
    config = {
      packageOverrides = pkgs: {
        vaapiIntel = pkgs.vaapiIntel.override {
          enableHybridCodec = true;
        };
      };
    };
  };

  services = {
    mullvad-vpn = {
      enable = true;
    };

    throttled = {
      enable = true;
    };

    tlp = {
      enable = true;

      settings = {
        TLP_ENABLE = 1;
        TLP_DEFAULT_MODE = "AC";

        CPU_SCALING_GOVERNOR_ON_AC = "performance";
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

        CPU_SCALING_MIN_FREQ_ON_AC = 2400000;
        CPU_SCALING_MAX_FREQ_ON_AC = 5000000;
        CPU_SCALING_MIN_FREQ_ON_BAT = 1500000;
        CPU_SCALING_MAX_FREQ_ON_BAT = 4000000;

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

    undervolt = {
      enable = true;
      coreOffset = -140;
      gpuOffset = -100;
    };
  };

  sops = {
    secrets = {
      backup-passphrase = {
        key = "backup-passphrase--lenovo";
      };
    };
  };

  system = {
    stateVersion = "20.09";
  };
}
