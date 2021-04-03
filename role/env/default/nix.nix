{ pkgs, ... }: {
  imports = [
    ./nix/distributed-builds.nix
  ];

  nix = {
    package = pkgs.nixUnstable;

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
