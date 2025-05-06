{ pkgs, lib, ... }:
{
  environment = {
    systemPackages = with pkgs; [
      rustup
    ];
  };

  home-manager.users.pwy = {
    home = {
      packages =
        with pkgs;
        [
          cargo-expand
          cargo-license
          cargo-nextest
          rust-analyzer
          simavr
        ]
        ++ (lib.optionals pkgs.stdenv.isLinux [
          avrdude
          pkgsCross.avr.buildPackages.gcc
          pkgsCross.avr.buildPackages.gdb
        ]);

      file = {
        ".cargo/config.toml".text = ''
          [target.x86_64-unknown-linux-gnu]
          rustflags = [
              "-C", "link-arg=-fuse-ld=lld",
          ]
        '';
      };
    };
  };
}
