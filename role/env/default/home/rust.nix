{ pkgs, ... }:
let
  sccache-wrapper = pkgs.writeShellScriptBin "sccache-wrapper" ''
    sccache="${pkgs.sccache}/bin/sccache"

    export SCCACHE_BUCKET=sccache
    export SCCACHE_ENDPOINT=$(cat /run/secrets/sccache-endpoint)
    export SCCACHE_S3_USE_SSL=true
    export AWS_ACCESS_KEY_ID=$(cat /run/secrets/sccache-key)
    export AWS_SECRET_ACCESS_KEY=$(cat /run/secrets/sccache-key)

    "$sccache" "$@"
  '';

in
{
  home-manager.users.pwy = {
    home = {
      file = {
        ".cargo/config".text = ''
          [build]
          rustc-wrapper = "${sccache-wrapper}/bin/sccache-wrapper"

          [target.x86_64-unknown-linux-gnu]
          rustflags = [
              "-C", "link-arg=-fuse-ld=lld",
          ]
        '';
      };

      packages = with pkgs; [
        cmake
        gcc
        gnumake
        lld
        rustup
        sccache-wrapper
      ];
    };
  };
}
