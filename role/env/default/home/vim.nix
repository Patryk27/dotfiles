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

          au BufEnter github.com_*.txt set filetype=markdown
          au BufEnter git.anixe.pl_*.txt set filetype=markdown

          let g:firenvim_config = {
              \ 'globalSettings': {
                  \ 'alt': 'all',
              \  },
              \ 'localSettings': {
                  \ '.*': {
                      \ 'cmdline': 'neovim',
                      \ 'content': 'text',
                      \ 'priority': 0,
                      \ 'selector': 'textarea',
                      \ 'takeover': 'never',
                  \ },
              \ }
          \ }
        '';
      };
    };
  };
}
