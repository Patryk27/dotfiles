{ pkgs, user, ... }: {
  home-manager.users."${user}" = {
    home = {
      packages = with pkgs; [
        cmake
        gnumake
        ninja
      ] ++ (if pkgs.stdenv.isLinux then [
        clang
        clang-tools
      ] else [ ]);
    };
  };
}
