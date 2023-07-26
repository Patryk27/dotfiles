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
        ".cargo/config".text = ''
          [target.x86_64-unknown-linux-gnu]
          rustflags = [
              "-C", "link-arg=-fuse-ld=lld",
          ]
        '';
      };
    };
  };
}