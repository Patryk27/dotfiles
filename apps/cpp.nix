{ pkgs, lib, ... }:
{
  home-manager.users.pwy = {
    home = {
      packages =
        with pkgs;
        [
          cmake
          gnumake
          ninja
        ]
        ++ (lib.optionals pkgs.stdenv.isLinux [
          clang
          clang-tools
        ]);
    };
  };
}
