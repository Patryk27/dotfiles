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
        kicad
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
    hostId = "8be46994";
    hostName = "lenovo";

    firewall = {
      allowedTCPPorts = [
        1313 # Hugo
      ];
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
      backup-passphrase = {
        key = "backup-passphrase--lenovo";
        owner = "pwy";
      };
    };
  };

  system = {
    stateVersion = "20.09";
  };
}
