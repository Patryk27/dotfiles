{ pkgs, ... }: {
  environment = {
    systemPackages = with pkgs; [
      nil
      nix-index
      nixpkgs-fmt
    ];
  };

  nix = {
    distributedBuilds = true;

    settings = {
      builders-use-substitutes = true;
      experimental-features = [ "ca-derivations" "flakes" "nix-command" ];
      sandbox = true;

      trusted-users = [
        "builder"
        "root"
        "@wheel"
      ];
    };
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };

  services = {
    lorri = {
      enable = true;
    };
  };
}
