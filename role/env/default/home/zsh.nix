{ pkgs, ... }:
let
  autorun = ./zsh/autorun;
  p10k-config = ./zsh/p10k.zsh;

  k = builtins.fetchGit {
    url = "https://github.com/supercrabtree/k.git";
    rev = "e2bfbaf3b8ca92d6ffc4280211805ce4b8a8c19e";
  };

in
{
  environment = {
    # For autosuggestions
    pathsToLink = [ "/share/zsh" ];
  };

  fonts = {
    fonts = with pkgs; [
      # For powerlevel10k
      powerline-fonts
    ];
  };

  programs = {
    autojump = {
      enable = true;
    };
  };

  home-manager.users.pwy = { pkgs, ... }: {
    home = {
      file = {
        ".p10k.zsh".source = ./zsh/p10k.zsh;
      };

      packages = with pkgs; [
        # For fzf-history-widget
        perl
      ];
    };

    programs = {
      zsh = {
        enable = true;
        autocd = true;
        enableAutosuggestions = true;
        enableCompletion = true;

        initExtraBeforeCompInit = ''
          if [[ "$TERM" == "dumb" ]]; then
              unsetopt zle
              unsetopt prompt_cr
              unsetopt prompt_subst
              PS1='$ '
              return
          fi
        '';

        history = {
          share = false;
        };

        oh-my-zsh = {
          enable = true;

          plugins = [
            "autojump"
            "docker"
            "docker-compose"
            "extract"
            "fzf"
            "git"
          ];

          extraConfig = ''
            DISABLE_AUTO_TITLE="true"
            COMPLETION_WAITING_DOTS="true"
            FZF_BASE="$(fzf-share)"

            # Initialize hook for lorri
            eval "$(direnv hook zsh)"

            # Initialize theme
            source ${p10k-config}
            source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme

            # Initialize other plugins
            source ${k}/k.sh
            source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

            ${pkgs.any-nix-shell}/bin/any-nix-shell zsh --info-right | source /dev/stdin

            # Launch autorun scripts
            source ${autorun}/_autorun
          '';
        };
      };
    };
  };
}
