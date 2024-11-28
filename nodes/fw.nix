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

    user = "pwy";
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

        environment = {
          systemPackages = with pkgs; [
            anki-bin
            btop
            chromium
            colmap
            dbeaver-bin
            discord
            firefox
            inkscape
            iw
            kdenlive
            libreoffice-qt
            lm_sensors
            moonlight-qt
            nethack
            nixpkgs-review
            obs-studio
            opensplat
            powertop
            slack
            spirv-tools
            spotify
            unnethack
            vlc
            wineWowPackages.waylandFull
            winetricks
            yt-dlp
          ];
        };

        home-manager = {
          users = {
            pwy = {
              programs = {
                git = {
                  userEmail = "pwychowaniec@pm.me";
                };
              };
            };
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
