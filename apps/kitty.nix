{ pkgs, ... }: {
  home-manager.users.pwy = {
    programs = {
      kitty = {
        enable = true;

        font = {
          name = "Iosevka Custom Light";
          size = 15;
        };

        extraConfig = ''
          include ${pkgs.kitty-themes}/themes/gruvbox-dark.conf
          enable_audio_bell no
          placement_strategy top-left
          confirm_os_window_close 0
        '';
      };
    };
  };
}
