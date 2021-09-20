{ pkgs, ... }: {
  fonts = {
    fonts = with pkgs; [
      emacs-all-the-icons-fonts
    ];
  };

  home-manager.users.pwy = { config, ... }: {
    home = {
      file = {
        ".doom.d" = {
          source = ./emacs/doom.d;
        };

        ".emacs.d" = {
          source = pkgs.doom-emacs;
        };
      };

      packages =
        let
          emacs' = pkgs.emacsPgtkGcc.overrideAttrs (attrs: {
            patches = [
              ./emacs/patch/regex.patch
            ];
          });

        in
        with pkgs; [
          ((emacsPackagesGen emacs').emacsWithPackages (epkgs: [
            epkgs.vterm
          ]))

          (aspellWithDicts (dicts: with dicts; [
            en
            en-computers
            en-science
            pl
          ]))

          rust-analyzer
        ];

      sessionVariables = {
        DOOMLOCALDIR = "${config.xdg.cacheHome}/.doom.local";
      };
    };
  };
}
