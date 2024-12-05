{
  pkgs,
  inputs,
  user,
  ...
}:
{
  fonts = {
    packages = with pkgs; [
      emacs-all-the-icons-fonts
    ];
  };

  home-manager.users."${user}" = {
    home = {
      packages = with pkgs; [
        (
          if pkgs.stdenv.isLinux then
            emacs30-pgtk
          else
            (emacs29-macport.overrideAttrs (old: {
              src = inputs.emacs-mac;
              version = "29.4";
            }))
        )

        (aspellWithDicts (
          dicts: with dicts; [
            en
            en-computers
            en-science
            pl
            sv
          ]
        ))

        (stdenv.mkDerivation {
          name = "epdfinfo";
          phases = "installPhase";

          installPhase = ''
            mkdir -p $out/bin
            ln -s $(${pkgs.findutils}/bin/find ${pkgs.emacsPackages.pdf-tools}/ -name epdfinfo) $out/bin/
          '';
        })

        (python3.withPackages (
          p:
          (with p; [
            python-lsp-server
          ])
        ))

        fd
        libtool
      ];

      file =
        let
          render =
            path:
            builtins.replaceStrings
              [
                "%llvm-mode%"
              ]
              [
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

      sessionVariables = {
        LSP_USE_PLISTS = "true";
      };
    };
  };
}
