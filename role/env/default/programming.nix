{ pkgs, ... }: {
  boot = {
    kernel = {
      sysctl = {
        "kernel.perf_event_paranoid" = -1;
      };
    };
  };

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
        ansible
        arcanist
        binutils
        clang-tools
        cmake
        gcc
        gdb
        gnumake
        imagemagick
        jq
        kcachegrind
        linuxPackages.perf
        lld
        ninja
        postgresql
        python
        python3
        ravedude
        rr
        rustup
        sqlite
        valgrind
      ];
    };
  };
}
