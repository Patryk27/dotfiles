{
  pkgs,
  lib,
  user,
  ...
}:
{
  home-manager.users."${user}" = {
    home = {
      packages = lib.mkIf pkgs.stdenv.isLinux (
        with pkgs;
        [
          gdb
          kcachegrind
          linuxPackages.perf
          valgrind
          wireshark
        ]
      );
    };
  };
}
