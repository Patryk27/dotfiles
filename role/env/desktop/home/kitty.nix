{ pkgs, ... }: {
  home-manager.users.pwy = {
    programs = {
      kitty = {
        enable = true;

        font = {
          name = "Iosevka Custom Light";
          size = 13;
        };

        extraConfig = ''
          include ${pkgs.kitty-themes}/themes/rose-pine-dawn.conf
          enable_audio_bell no
          placement_strategy top-left
        '';
      };
    };
  };
}
