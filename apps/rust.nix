{ pkgs, ... }:
{
  environment = {
    systemPackages = with pkgs; [
      rustup
    ];
  };

  home-manager.users.pwy = {
    home = {
      packages = with pkgs; [
        cargo-duplicates
        cargo-expand
        cargo-insta
        cargo-license
        cargo-nextest
        cargo-outdated
        rust-analyzer
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
