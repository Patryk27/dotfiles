{ pkgs, ... }: {
  home-manager.users.root = {
    programs = {
      ssh = {
        enable = true;

        extraOptionOverrides = {
          "Match Originalhost builder" = ''
            Exec "nmcli | rg Dziupla5"
              Hostname 192.168.1.200
          '';
        };

        matchBlocks = {
          builder = {
            hostname = "10.24.1.2";
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
        hostName = "builder";
        system = "i686-linux";
        maxJobs = 8;
        speedFactor = 5;
        supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
        mandatoryFeatures = [ ];
      }

      {
        hostName = "builder";
        system = "x86_64-linux";
        maxJobs = 8;
        speedFactor = 5;
        supportedFeatures = [ "nixos-test" "benchmark" "big-parallel" "kvm" ];
        mandatoryFeatures = [ ];
      }
    ];

    settings = {
      trusted-public-keys = [
        "builder:vj2KhujwD56FS5m1E/MNfUjlBGFLzWEc+ESHbiTJQ18="
      ];

      trusted-substituters = [
        "ssh-ng://builder"
      ];
    };
  };
}
