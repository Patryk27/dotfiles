{
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
    home-manager.nixosModules.home-manager
    nixos-hardware.nixosModules.framework-16-7040-amd

    (
      { pkgs, ... }:
      {
        imports = [
          ../roles/base.nix

          ./fw/backup.nix
          ./fw/gui.nix
          ./fw/hw.nix
          ./fw/iphone.nix
          ./fw/net.nix
          ./fw/ssh.nix
          ./fw/user.nix
          ./fw/virt.nix
        ];

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
            anki-bin
            btop
            chromium
            darktable
            dbeaver-bin
            discord
            firefox
            httm
            inkscape
            iw
            kdenlive
            libreoffice-qt
            lm_sensors
            moonlight-qt
            nixpkgs-review
            obs-studio
            powertop
            spirv-tools
            spotify
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
            permittedInsecurePackages = [
              "freeimage-unstable-2021-11-01"
            ];
          };
        };

        programs = {
          steam = {
            enable = true;
          };
        };

        services = {
          fwupd = {
            enable = true;
          };
        };

        system = {
          stateVersion = "24.05";
        };

        systemd = {
          services = {
            disable-led = {
              script = ''
                ${pkgs.fw-ectool}/bin/ectool led power off
              '';

              wantedBy = [ "multi-user.target" ];
            };
          };

          timers = {
            fwupd-refresh = {
              enable = false;
            };
          };
        };

        time = {
          timeZone = "Europe/Warsaw";
        };
      }
    )
  ];
}
