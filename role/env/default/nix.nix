{ pkgs, ... }: {
  imports = [
    ./nix/distributed-builds.nix
  ];

  environment = {
    systemPackages = with pkgs; [
      nix-index
      nixpkgs-fmt
    ];
  };

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    settings = {
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
}
