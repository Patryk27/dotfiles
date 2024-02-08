{ pkgs, ... }: {
  environment = {
    systemPackages = with pkgs; [
      (emacs29-macport.overrideAttrs (old: {
        src = pkgs.sources.emacs-mac;
      }))

      (aspellWithDicts (dicts: with dicts; [
        en
        en-computers
        en-science
        pl
        sv
      ]))
    ];
  };

  home-manager.users.PWY = {
    home = {
      file =
        let
          render = path:
            builtins.replaceStrings [
              "%ion-mode%"
              "%parinfer%"
            ] [
              (toString ./emacs/vendor/ion-mode.el)
              (toString "${pkgs.parinfer-rust}/lib/libparinfer_rust.dylib")
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
