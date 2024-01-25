{ pkgs, ... }: {
  imports = [
    ../apps/csharp.nix
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
      aria
      audacity
      bat
      cmake
      curl
      fd
      fzf
      github-cli
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
      spirv-tools
      sqlite
      sshfs
      watch
      wget
      wrk
    ];
  };

  fonts = {
    fontDir = {
      enable = true;
    };
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
