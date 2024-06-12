{ pkgs, ... }: {
  home-manager.users.pwy = {
    home = {
      packages = with pkgs; [
        clang-tools
        cmake
        gcc
        gnumake
        ninja
      ];
    };
  };
}
