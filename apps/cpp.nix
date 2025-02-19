{ pkgs, ... }:
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
        ++ (
          if pkgs.stdenv.isLinux then
            [
              clang
              clang-tools
            ]
          else
            [ ]
        );
    };
  };
}
