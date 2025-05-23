{ pkgs, lib, ... }:
{
  home-manager.users.pwy = {
    home = {
      packages = lib.optionals pkgs.stdenv.isLinux (
        with pkgs;
        [
          avrdude
          pkgsCross.avr.buildPackages.gcc
          pkgsCross.avr.buildPackages.gdb
          simavr
        ]
      );
    };
  };
}
