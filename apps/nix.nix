{ pkgs, ... }: {
  home-manager.users.root = {
    programs = {
      ssh = {
        enable = true;

        matchBlocks = {
          warp = {
            hostname = "10.24.1.2";
            port = 33000;
            user = "pwy";
          };
        };
      };
    };
  };

  environment = {
    systemPackages = with pkgs; [
      nix-index
      nixpkgs-fmt
    ];
  };

  nix = {
    package = pkgs.nix;
    distributedBuilds = true;

    extraOptions = ''
      sandbox = true
      builders-use-substitutes = true
      experimental-features = nix-command flakes
      extra-platforms = x86_64-darwin aarch64-darwin
    '';

    buildMachines = [
      {
        hostName = "warp";
        system = "x86_64-linux";
        maxJobs = 8;
        speedFactor = 1;
        supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
        mandatoryFeatures = [ ];
      }
    ];

    settings = {
      trusted-users = [
        "builder"
        "root"
        "@wheel"
      ];

      trusted-public-keys = [
        "warp:vj2KhujwD56FS5m1E/MNfUjlBGFLzWEc+ESHbiTJQ18="
      ];

      trusted-substituters = [
        "ssh-ng://warp"
      ];
    };
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };

  services = {
    nix-daemon = {
      enable = true;
    };
  };
}
