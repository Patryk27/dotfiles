{ pkgs, ... }: {
  home-manager.users.root = { lib, ... }: {
    programs = {
      ssh = {
        enable = true;

        matchBlocks = {
          warp = {
            port = 33000;
            user = "pwy";
          };

          warp--local = {
            match = ''OriginalHost warp Exec "networksetup -getairportnetwork en0 | grep -q 'Desafinado'"'';
            hostname = "192.168.1.200";
          };

          warp--wg = lib.hm.dag.entryAfter [ "warp--local" ] {
            match = "OriginalHost warp";
            hostname = "10.24.1.2";
          };
        };
      };
    };
  };

  environment = {
    systemPackages = with pkgs; [
      nil
      niv
      nix-index
      nixos-rebuild
      nixpkgs-fmt
    ];
  };

  nix = {
    package = pkgs.nix.overrideAttrs
      (final: prev: {
        patches = prev.patches ++ [
          # TODO https://github.com/NixOS/nix/issues/9625
          ./nix/openssl-sandbox.patch
        ];
      });

    distributedBuilds = true;

    settings = {
      builders-use-substitutes = true;
      experimental-features = [ "ca-derivations" "flakes" "nix-command" ];
      extra-platforms = [ "x86_64-darwin" "aarch64-darwin" ];
      sandbox = true;

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
