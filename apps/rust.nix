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
        cargo-fuzz
        cargo-insta
        cargo-license
        cargo-nextest
        cargo-outdated
        rust-analyzer
      ];

      file = {
        ".cargo/config.toml".text = ''
          [registries.proton]
          index = "sparse+https://rust.gitlab-pages.protontech.ch/shared/registry/index/"

          [target.x86_64-unknown-linux-gnu]
          rustflags = [
              "-C", "link-arg=-fuse-ld=lld",
          ]
        '';
      };
    };
  };
}
