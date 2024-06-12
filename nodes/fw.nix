# TODO swap

{ config, pkgs, ... }: {
  imports = [
    ../apps/csharp.nix
    ../apps/direnv.nix
    ../apps/emacs.nix
    ../apps/git.nix
    ../apps/gpg.nix
    ../apps/kitty.nix
    ../apps/lorri.nix
    ../apps/nix.nix
    ../apps/res.nix
    ../apps/rust.nix
    ../apps/ssh.nix
    ../apps/vim.nix
    ../apps/zsh.nix
  ];

  boot = {
    kernelPackages = config.boot.zfs.package.latestCompatibleLinuxPackages;
    supportedFilesystems = [ "zfs" ];

    kernelParams = [
      "mitigations=off"
    ];

    initrd = {
      availableKernelModules = [
        "nvme"
        "xhci_pci"
        "thunderbolt"
        "usb_storage"
        "usbhid"
        "sd_mod"
      ];

      kernelModules = [
        "kvm-amd"
      ];

      luks = {
        devices = {
          root = {
            device = "/dev/nvme0n1p2";
            preLVM = true;
          };
        };
      };
    };

    loader = {
      grub = {
        enable = true;
        zfsSupport = true;
        efiSupport = true;
        efiInstallAsRemovable = true;
        enableCryptodisk = true;

        mirroredBoots = [
          {
            devices = [ "nodev" ];
            path = "/boot";
          }
        ];
      };
    };

    zfs = {
      devNodes = "/dev/mapper";
    };
  };

  environment = {
    systemPackages = with pkgs; [
      aria
      audacity
      clang-tools
      cmake
      curl
      fd
      firefox
      fzf
      gcc
      github-cli
      gnome3.gnome-tweaks
      gnumake
      google-cloud-sdk
      htop
      jq
      just
      libxml2
      lld
      moonlight-qt
      ncdu
      nethack
      ninja
      nmap
      postgresql
      python3
      ripgrep
      rsync
      slack
      spirv-tools
      spotify
      sqlite
      sshfs
      unzip
      virt-manager
      watch
      wget
      wrk
    ];

    sessionVariables = {
      EDITOR = "vim";
      NIXOS_OZONE_WL = "1";
    };
  };

  fileSystems = {
    "/" = {
      device = "rpool";
      fsType = "zfs";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/3F33-58F9";
      fsType = "vfat";
      options = [ "fmask=0022" "dmask=0022" ];
    };

    "/nix" = {
      device = "rpool/nix";
      fsType = "zfs";
    };
  };

  hardware = {
    enableRedistributableFirmware = true;

    cpu = {
      amd = {
        updateMicrocode = true;
      };
    };
  };

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;

    users = {
      root = {
        home = {
          username = "root";
          stateVersion = "24.05";
        };
      };

      pwy = {
        home = {
          username = "pwy";
          stateVersion = "24.05";
        };
      };
    };
  };

  networking = {
    hostId = "c7a81ba2";
    hostName = "pfw";
  };

  security = {
    sudo = {
      wheelNeedsPassword = false;
    };
  };

  services = {
    fprintd = {
      enable = true;
    };

    zfs = {
      autoScrub = {
        enable = true;
      };

      autoSnapshot = {
        enable = true;
        flags = "-k -p --utc";
        monthly = 0;
      };
    };

    xserver = {
      enable = true;

      displayManager = {
        gdm = {
          enable = true;
          wayland = true;
        };
      };

      desktopManager = {
        gnome = {
          enable = true;
        };
      };
    };
  };

  time = {
    timeZone = "Europe/Warsaw";
  };

  users = {
    users = {
      pwy = {
        home = "/home/pwy";
        extraGroups = [ "docker" "libvirtd" "wheel" ];
        isNormalUser = true;
      };
    };
  };

  virtualisation = {
    docker = {
      enable = true;
    };

    libvirtd = {
      enable = true;

      qemu = {
        package = pkgs.qemu_kvm;
        runAsRoot = true;

        swtpm = {
          enable = true;
        };

        ovmf = {
          enable = true;

          packages = [
            (pkgs.OVMF.override {
              secureBoot = true;
              tpmSupport = true;
            }).fd
          ];
        };
      };
    };
  };
}
