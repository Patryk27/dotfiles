{ pkgs, ... }: {
  home-manager.users.PWY = {
    programs = {
      kitty = {
        enable = true;

        font = {
          name = "JetBrains Mono Light";
          size = 14;
        };

        extraConfig = ''
          include ${pkgs.kitty-themes}/themes/gruvbox-dark.conf
          enable_audio_bell no
          placement_strategy top-left
          confirm_os_window_close 0
          disable_ligatures always
          map cmd+h no_op
          map cmd+k no_op
        '';
      };
    };
  };
}
