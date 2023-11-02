{ pkgs, ... }: {
  environment = {
    systemPackages = with pkgs; [
      rustup
    ];
  };

  home-manager.users.PWY = {
    home = {
      packages = with pkgs; [
        rust-analyzer
      ];

      file = {
        ".cargo/config".text = ''
          [net]
          git-fetch-with-cli = true

          [target.x86_64-unknown-linux-gnu]
          rustflags = [
              "-C", "link-arg=-fuse-ld=lld",
          ]
        '';
      };
    };
  };
}
