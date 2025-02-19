{ pkgs, ... }:
{
  programs = {
    zsh = {
      enable = true;
    };
  };

  home-manager.users.pwy = {
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
        };
      };
    };
  };

  users = {
    users = {
      pwy = {
        shell = pkgs.zsh;
      };
    };
  };
}
