{ pkgs, ... }: {
  environment = {
    systemPackages = with pkgs; [
      rustup
    ];
  };

  home-manager.users.pwy = {
    home = {
      packages = with pkgs; [
        rust-analyzer
      ];

      file = {
        ".cargo/config.toml".text = ''
          [target.x86_64-unknown-linux-gnu]
          rustflags = [
              "-C", "link-arg=-fuse-ld=lld",
              "-Zlinker-features=-lld"
          ]
        '';
      };
    };
  };
}
