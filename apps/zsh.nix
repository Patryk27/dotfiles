{ pkgs, user, ... }:
{
  programs = {
    zsh = {
      enable = true;
    };
  };

  home-manager.users."${user}" = {
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
      "${user}" = {
        shell = pkgs.zsh;
      };
    };
  };
}
