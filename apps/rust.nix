{ pkgs, user, ... }:
{
  environment = {
    systemPackages = with pkgs; [
      rustup
    ];
  };

  home-manager.users."${user}" = {
    home = {
      packages = with pkgs; [
        avrdude
        cargo-expand
        cargo-license
        cargo-nextest
        pkgsCross.avr.buildPackages.gcc
        pkgsCross.avr.buildPackages.gdb
        rust-analyzer
        simavr
      ];

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
