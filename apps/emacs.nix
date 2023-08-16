{ pkgs, ... }: {
  fonts = {
    fonts = with pkgs; [
      emacs-all-the-icons-fonts
    ];
  };

  environment = {
    systemPackages = with pkgs; [
      (aspellWithDicts (dicts: with dicts; [
        en
        en-computers
        en-science
        pl
        sv
      ]))

      omnisharp-roslyn
      rnix-lsp
    ];
  };

  home-manager.users.PWY = {
    home = {
      file = {
        ".doom.d" = {
          source = ./emacs/doom.d;
        };

        ".emacs.d" = {
          source = pkgs.sources.doom-emacs;
        };
      };

      sessionVariables = {
        DOOMLOCALDIR = "/Users/pwy/.cache/.doom.local";
        DOOMPROFILELOADFILE = "/Users/pwy/.cache/profile-load.el";
      };
    };
  };
}
