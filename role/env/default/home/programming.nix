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
        arcanist
        binutils
        clang-tools
        cmake
        gcc
        gdb
        gnumake
        jq
        kcachegrind
        linuxPackages.perf
        lld
        ninja
        python
        python3
        rr
        rustup
        valgrind
      ];
    };
  };
}
