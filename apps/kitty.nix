{ ... }: {
  home-manager.users.pwy = {
    programs = {
      kitty = {
        enable = true;

        font = {
          name = "Berkeley Mono";
          size = 14;
        };

        extraConfig = ''
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
