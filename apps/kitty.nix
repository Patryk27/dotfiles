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
        '';
      };
    };
  };
}
