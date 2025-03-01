{ pkgs, lib, ... }:
{
  home-manager.users.pwy = {
    home = {
      packages = lib.mkIf pkgs.stdenv.isLinux (
        with pkgs;
        [
          gdb
          kdePackages.kcachegrind
          linuxPackages.perf
          valgrind
          wireshark
        ]
      );
    };
  };
}
