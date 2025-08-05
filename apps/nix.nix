{ pkgs, inputs, ... }:
{
  environment = {
    systemPackages = with pkgs; [
      nil
      nix-index
      nixfmt-rfc-style
    ];
  };

  nix = {
    distributedBuilds = true;

    nixPath = [
      "nixpkgs=${inputs.nixpkgs}"
    ];

    registry = {
      nixpkgs = {
        flake = inputs.nixpkgs;
      };
    };

    settings = {
      builders-use-substitutes = true;

      experimental-features = [
        "ca-derivations"
        "cgroups"
        "flakes"
        "nix-command"
      ];

      sandbox = true;

      trusted-users = [
        "@wheel"
        "builder"
        "pwy"
        "root"
      ];
    };
  };

  nixpkgs = {
    config = {
      allowUnfree = true;

      # TODO
      permittedInsecurePackages = [
        "libsoup-2.74.3"
      ];
    };
  };

  services = {
    lorri = {
      enable = true;
    };
  };
}
