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
            device = "/dev/disk/by-uuid/5ccd6167-0771-4196-becc-c6d7ad481d1a";
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
      screen
    ];
  };

  hardware = {
    opengl = {
      extraPackages = with pkgs; [
        intel-media-driver
        libglvnd
        libvdpau-va-gl
        vaapiIntel
        mesa
        vaapiVdpau
      ];
    };
  };

  home-manager.users.pwy = {
    home = {
      packages = with pkgs; [
        (import ../role/app/backup.nix {
          inherit pkgs;
          repo = "pwy@eric-archive:/home/pwy/backup-lenovo";
        })

        darktable
        geeqie
        gocryptfs
        inkscape
        kicad
        mullvad
        obs-studio
        rawtherapee
        spotify
        virt-manager
      ];

      sessionVariables = {
        MESA_LOADER_DRIVER_OVERRIDE = "iris";
      };
    };
  };

  networking = {
    hostId = "640ada54";
    hostName = "lenovo";

    wireguard = {
      interfaces = {
        wg-fort = {
          ips = [ "10.24.1.10/24" ];
          listenPort = 51820;
          privateKeyFile = "/run/secrets/wg-fort:private-key";

          peers = [
            # edge
            {
              publicKey = "GwhWP0DClVw9fY7PJidPuZfOzBhxhcnjTnO+8i1Z50w=";
              allowedIPs = [ "10.24.1.0/24" ];
              endpoint = "185.238.72.182:51820";
              persistentKeepalive = 10;
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
    davfs2 = {
      enable = true;
    };

    mullvad-vpn = {
      enable = true;
    };

    printing = {
      enable = true;
      drivers = [ pkgs.hplip ];
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
        CPU_SCALING_MIN_FREQ_ON_AC = 3000000;
        CPU_SCALING_MAX_FREQ_ON_AC = 5000000;
        CPU_BOOST_ON_AC = 1;
        CPU_ENERGY_PERF_POLICY_ON_AC = "performance";

        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";
        CPU_SCALING_MIN_FREQ_ON_BAT = 800000;
        CPU_SCALING_MAX_FREQ_ON_BAT = 3000000;
        CPU_BOOST_ON_BAT = 0;
        CPU_ENERGY_PERF_POLICY_ON_BAT = "balance_power";
      };
    };

    udev = {
      extraRules = ''
        # USBasp
        ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="05dc", MODE="0666"

        # Bus Pirate
        ATTRS{idVendor}=="0403", ATTRS{idProduct}=="6001", MODE="0666"

        # Arduino
        KERNEL=="ttyACM[0-9]*", MODE="0666"
      '';
    };

    undervolt = {
      enable = true;
      coreOffset = -140;
      gpuOffset = -100;
    };
  };

  sops = {
    secrets = {
      "backup:passphrase" = {
        key = "backup:passphrase:lenovo";
        owner = "pwy";
      };

      "wg-fort:private-key" = {
        key = "wg-fort:private-key:lenovo";
      };
    };
  };

  system = {
    stateVersion = "21.11";
  };
}
