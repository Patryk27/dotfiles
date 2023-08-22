{ config, pkgs, ... }: {
  imports = [
    ../apps/direnv.nix
    ../apps/emacs.nix
    ../apps/git.nix
    ../apps/gpg.nix
    ../apps/kitty.nix
    ../apps/lorri.nix
    ../apps/mnt.nix
    ../apps/nix.nix
    ../apps/rust.nix
    ../apps/skhd.nix
    ../apps/ssh.nix
    ../apps/vim.nix
    ../apps/yabai.nix
    ../apps/zsh.nix
  ];

  environment = {
    systemPackages = with pkgs; [
      arcanist
      aria
      cmake
      curl
      fd
      fzf
      github-cli
      gnumake
      google-cloud-sdk
      htop
      jq
      libxml2
      lld
      ncdu
      ninja
      niv
      nixos-rebuild
      nmap
      postgresql
      python3
      ripgrep
      spirv-tools
      sqlite
      sshfs
      wget
      wrk
    ];
  };

  fonts = {
    fontDir = {
      enable = true;
    };

    fonts =
      let
        iosevka-custom = pkgs.iosevka.override {
          set = "custom";

          privateBuildPlan = {
            family = "Iosevka Custom";
            spacing = "term";
            serifs = "sans";
            no-cv-ss = true;
            no-ligation = true;

            metric-override = {
              leading = 1025;
            };

            widths = {
              normal = {
                menu = 5;
                css = "normal";
                shape = 480;
              };
            };

            variants = {
              inherits = "ss08";
            };
          };
        };

      in
      with pkgs; [
        iosevka-custom
      ];
  };

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;

    users = {
      root = {
        home = {
          stateVersion = "19.09";
        };
      };

      PWY = {
        home = {
          stateVersion = "23.05";
          username = "PWY";

          sessionVariables = {
            EDITOR = "vim";
          };
        };
      };
    };
  };

  system = {
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToEscape = true;
    };
  };

  users = {
    users = {
      PWY = {
        home = "/Users/PWY";
      };

      root = {
        home = "/var/root";
      };
    };
  };
}
