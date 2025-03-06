{
  inputs = {
    agenix = {
      url = "github:ryantm/agenix";
    };

    emacs-mac = {
      url = "https://bitbucket.org/mituharu/emacs-mac/get/7cc5e67629363d9e98f65e4e652f83bb4e0ee674.zip";
      flake = false;
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
      url = "github:nixos/nixpkgs/nixos-unstable";
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
