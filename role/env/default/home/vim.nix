{ pkgs, ... }: {
  home-manager.users.pwy = {
    programs = {
      neovim = {
        enable = true;
        viAlias = true;
        vimAlias = true;

        extraConfig = ''
          set tabstop=4
          set shiftwidth=4
          set expandtab
          set nobackup

          filetype plugin indent on
        '';
      };
    };
  };
}
