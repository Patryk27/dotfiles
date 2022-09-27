{ pkgs, ... }: {
  home-manager.users.pwy = {
    programs = {
      rofi = {
        enable = true;
        font = "Iosevka Custom 24";
      };
    };
  };
}
