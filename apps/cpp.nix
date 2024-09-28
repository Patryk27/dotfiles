{ pkgs, user, ... }: {
  home-manager.users."${user}" = {
    home = {
      packages = with pkgs; [
        clang
        clang-tools
        cmake
        gnumake
        ninja
      ];
    };
  };
}
