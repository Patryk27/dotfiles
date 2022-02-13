{ pkgs, ... }: {
  home-manager.users.root = {
    programs = {
      ssh = {
        enable = true;

        matchBlocks = {
          eric-builder = {
            hostname = "188.122.2.73";
            port = 33001;
            user = "builder";
          };
        };
      };
    };
  };

  nix = {
    distributedBuilds = true;

    extraOptions = ''
      builders-use-substitutes = true
    '';

    buildMachines = [
      {
        hostName = "eric-builder";
        system = "i686-linux";
        maxJobs = 28;
        speedFactor = 10;
        supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
        mandatoryFeatures = [ ];
      }

      {
        hostName = "eric-builder";
        system = "x86_64-linux";
        maxJobs = 28;
        speedFactor = 10;
        supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
        mandatoryFeatures = [ ];
      }
    ];

    settings = {
      trusted-substituters = [
        "ssh://eric-builder"
      ];

      trusted-public-keys = [
        "eric-builder:vj2KhujwD56FS5m1E/MNfUjlBGFLzWEc+ESHbiTJQ18="
      ];
    };
  };
}
