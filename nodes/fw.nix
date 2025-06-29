{
  agenix,
  home-manager,
  nixpkgs,
  nixos-hardware,
  ...
}@inputs:

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";

  specialArgs = {
    inherit inputs;
  };

  modules = [
    agenix.nixosModules.default
    home-manager.nixosModules.home-manager
    nixos-hardware.nixosModules.framework-16-7040-amd

    (
      { pkgs, ... }:
      {
        imports = [
          ../roles/base.nix

          ./fw/backup.nix
          ./fw/containers.nix
          ./fw/gui.nix
          ./fw/hw.nix
          ./fw/iphone.nix
          ./fw/net.nix
          ./fw/ssh.nix
          ./fw/user.nix
          ./fw/virt.nix
        ];

        age = {
          identityPaths = [
            "/secrets/age/key"
          ];
        };

        boot = {
          binfmt = {
            emulatedSystems = [
              "aarch64-linux"
              "armv6l-linux"
              "armv7l-linux"
              "powerpc64le-linux"
              "riscv64-linux"
              "s390x-linux"
            ];
          };
        };

        environment = {
          systemPackages = with pkgs; [
            agenix.packages.x86_64-linux.default
            anki-bin
            btop
            chromium
            darktable
            dbeaver-bin
            discord
            firefox
            httm
            imagemagick
            inkscape
            iperf3
            iw
            kdePackages.kdenlive
            libreoffice-qt
            lm_sensors
            moonlight-qt
            nixpkgs-review
            obs-studio
            powertop
            s-tui
            spirv-tools
            spotify
            stress
            vlc
            wineWowPackages.waylandFull
            winetricks
            yt-dlp
          ];
        };

        networking = {
          firewall = {
            enable = false;
          };
        };

        nixpkgs = {
          config = {
            rocmSupport = true;
          };
        };

        programs = {
          firejail = {
            enable = true;
          };

          steam = {
            enable = true;
          };
        };

        services = {
          davfs2 = {
            enable = true;
          };

          fwupd = {
            enable = true;
          };

          prometheus = {
            exporters = {
              node = {
                enable = true;
                enabledCollectors = [ "systemd" ];
              };

              zfs = {
                enable = true;
              };
            };
          };
        };

        system = {
          stateVersion = "24.05";
        };

        systemd = {
          mounts = [
            {
              what = "http://archive.lan/remote.php/webdav";
              where = "/mnt/archive";
              type = "davfs";
              options = "uid=1000,gid=100";
              description = "archive.lan";
            }
          ];

          services = {
            disable-led = {
              script = ''
                ${pkgs.fw-ectool}/bin/ectool led power off
              '';

              wantedBy = [ "multi-user.target" ];
            };
          };
        };

        time = {
          timeZone = "Europe/Stockholm";
        };
      }
    )
  ];
}
