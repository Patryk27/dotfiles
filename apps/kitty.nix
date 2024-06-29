{ ... }: {
  home-manager.users.pwy = {
    programs = {
      kitty = {
        enable = true;

        font = {
          name = "Berkeley Mono";
          size = 13;
        };

        extraConfig = ''
          enable_audio_bell no
          disable_ligatures always
          text_composition_strategy legacy

          map ctrl+tab no_op
          map ctrl+shift+[ no_op
          map ctrl+shift+] no_op
        '';
      };
    };
  };
}
