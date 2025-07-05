{
  inputs = {
    agenix = {
      url = "github:ryantm/agenix";
    };

    fw-fanctrl = {
      url = "github:TamtamHero/fw-fanctrl/packaging/nix";

      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };

    home-manager = {
      url = "github:nix-community/home-manager";

      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };

    nixos-hardware = {
      url = "github:NixOS/nixos-hardware";
    };

    nixpkgs = {
      url = "github:nixos/nixpkgs";
    };

    nix-darwin = {
      url = "github:LnL7/nix-darwin";

      inputs = {
        nixpkgs = {
          follows = "nixpkgs";
        };
      };
    };
  };

  outputs = inputs: {
    darwinConfigurations = {
      "Patryks-MacBook-Pro" = import ./nodes/mac.nix inputs;
    };

    nixosConfigurations = {
      fw = import ./nodes/fw.nix inputs;
    };
  };
}
