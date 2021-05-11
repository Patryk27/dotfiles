{ pkgs, ... }: {
  fonts = {
    fonts = with pkgs; [
      emacs-all-the-icons-fonts
    ];
  };

  home-manager.users.pwy = { config, ... }: {
    home = rec {
      file = {
        ".doom.d" = {
          source = ./emacs/doom.d;

          onChange = ''
            export DOOMLOCALDIR="${sessionVariables.DOOMLOCALDIR}"

            if [ -d "$DOOMLOCALDIR" ]; then
              ~/.emacs.d/bin/doom sync
            fi
          '';
        };

        ".emacs.d" = {
          source = pkgs.doom-emacs;

          onChange = ''
            export DOOMLOCALDIR="${sessionVariables.DOOMLOCALDIR}"

            if [ -d "$DOOMLOCALDIR" ]; then
              ~/.emacs.d/bin/doom -y sync -u
            else
              ~/.emacs.d/bin/doom -y install
            fi
          '';
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

          python3
          rust-analyzer
        ];

      sessionVariables = {
        DOOMLOCALDIR = "${config.xdg.cacheHome}/.doom.local";
      };
    };
  };
}
