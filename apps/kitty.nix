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
          disable_ligatures always
        '';
      };
    };
  };
}
