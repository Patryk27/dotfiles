{ pkgs, ... }: {
  programs = {
    zsh = {
      enable = true;
    };
  };

  home-manager.users.pwy = {
    home = {
      packages = with pkgs; [
        autojump

        # For fzf-history-widget
        perl
      ];
    };

    programs = {
      zsh = {
        enable = true;
        enableCompletion = true;

        autosuggestion = {
          enable = true;
        };

        history = {
          share = false;
        };

        oh-my-zsh = {
          enable = true;

          plugins = [
            "autojump"
            "extract"
            "fzf"
          ];

          extraConfig = ''
            source ${./zsh/init.zsh}
          '';
        };
      };
    };
  };
}
