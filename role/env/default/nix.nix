{ pkgs, ... }: {
  imports = [
    ./nix/distributed-builds.nix
  ];

  nix = {
    package = pkgs.nixUnstable.override {
      patches = [
        # TODO https://github.com/nix-community/nix-direnv/issues/113#issuecomment-921328351
        (pkgs.writeText "patch" ''
          diff --git a/src/nix/get-env.sh b/src/nix/get-env.sh
          index 42c806450..a8563c772 100644
          --- a/src/nix/get-env.sh
          +++ b/src/nix/get-env.sh
          @@ -8,6 +8,8 @@ if [[ -n $stdenv ]]; then
               source $stdenv/setup
           fi

          +unset -f isMachO
          +
           # Better to use compgen, but stdenv bash doesn't have it.
           __vars="$(declare -p)"
           __functions="$(declare -F)"
        '')
      ];
    };

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
