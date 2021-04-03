{ pkgs, ... }: {
  fonts = {
    fonts = with pkgs; [
      emacs-all-the-icons-fonts
    ];
  };

  home-manager.users.pwy = { ... }:
    let
      doom-sync = pkgs.writeShellScriptBin "doom-sync" ''
        set -ex

        if [[ ! -d ~/.emacs.d ]]; then
          git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
        fi

        cd ~/.emacs.d
        git fetch
        git checkout ${pkgs.doom-emacs.rev}
        ~/.emacs.d/bin/doom sync -up
      '';

    in
    {
      home = {
        file = {
          ".doom.d".source = ./emacs/doom.d;
        };

        packages = with pkgs; [
          ((emacsPackagesNgGen emacsPgtkGcc).emacsWithPackages (epkgs: [
            epkgs.vterm
          ]))

          (aspellWithDicts (dicts: with dicts; [
            en
            en-computers
            en-science
            pl
          ]))

          doom-sync
          python3
          rust-analyzer
        ];
      };
    };
}
