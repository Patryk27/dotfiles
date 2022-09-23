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
          source = pkgs.sources.doom-emacs;
        };
      };

      packages =
        let
          emacs'' = pkgs.emacsPgtkNativeComp.override {
            withImageMagick = true;
          };

          emacs' = emacs''.overrideAttrs (attrs: {
            src = pkgs.sources.emacs;

            patches = attrs.patches ++ [
              ./emacs/patch/regex.patch
              ./emacs/patch/synchronized-updates.patch
            ];
          });

        in
        with pkgs; [
          ((emacsPackagesFor emacs').emacsWithPackages (epkgs: [
            epkgs.vterm
          ]))

          (aspellWithDicts (dicts: with dicts; [
            en
            en-computers
            en-science
            pl
          ]))

          rnix-lsp
          rust-analyzer
        ];

      sessionVariables = {
        DOOMLOCALDIR = "${config.xdg.cacheHome}/.doom.local";
        DOOMPROFILELOADFILE = "${config.xdg.cacheHome}/profile-load.el";
      };
    };
  };
}
