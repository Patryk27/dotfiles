{ pkgs, ... }: {
  home-manager.users.pwy = {
    home = {
      file = {
        ".cargo/config".text = ''
          [target.x86_64-unknown-linux-gnu]
          rustflags = [
              "-C", "link-arg=-fuse-ld=lld",
          ]
        '';
      };

      packages = with pkgs; [
        binutils
        cmake
        gcc
        gdb
        gnumake
        jq
        linuxPackages.perf
        lld
        python
        rustup
        valgrind
      ];
    };
  };
}
