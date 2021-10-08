{ pkgs, ... }: {
  home-manager.users.pwy = {
    programs = {
      neovim = {
        enable = true;
        viAlias = true;
        vimAlias = true;
      };
    };
  };
}
