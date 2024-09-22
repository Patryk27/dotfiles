{ pkgs, ... }: {
  environment = {
    systemPackages = with pkgs; [
      emacs29-pgtk

      (aspellWithDicts (dicts: with dicts; [
        en
        en-computers
        en-science
        pl
        sv
      ]))

      (pkgs.stdenv.mkDerivation {
        name = "epdfinfo";
        phases = "installPhase";

        installPhase = ''
          mkdir -p $out/bin
          ln -s $(${pkgs.findutils}/bin/find ${pkgs.emacsPackages.pdf-tools}/ -name epdfinfo) $out/bin/
        '';
      })

      libtool
      fd
    ];

    sessionVariables = {
      LSP_USE_PLISTS = "true";
    };
  };

  fonts = {
    packages = with pkgs; [
      emacs-all-the-icons-fonts
    ];
  };

  home-manager.users.pwy = {
    home = {
      file =
        let
          render = path:
            builtins.replaceStrings [
              "%ion-mode%"
              "%llvm-mode%"
            ] [
              (toString ./emacs/vendor/ion-mode.el)
              (toString "${pkgs.llvm.src}/llvm/utils/emacs/llvm-mode.el")
            ]
              (builtins.readFile path);

        in
        {
          ".doom.d/config.el" = {
            text = render ./emacs/doom.d/config.el;
          };

          ".doom.d/init.el" = {
            text = render ./emacs/doom.d/init.el;
          };

          ".doom.d/packages.el" = {
            text = render ./emacs/doom.d/packages.el;
          };
        };
    };
  };
}
