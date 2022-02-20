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
    trustedUsers = [
      "builder"
      "root"
      "@wheel"
    ];

    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
    };
  };
}
