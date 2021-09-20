{ ... }: {
  imports = [
    ./home/git.nix
    ./home/gpg.nix
    ./home/lorri.nix
    ./home/programming.nix
    ./home/ssh.nix
    ./home/taskwarrior.nix
    ./home/vim.nix
    ./home/zsh.nix
  ];

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;

    users = {
      pwy = { pkgs, ... }: {
        home = {
          username = "pwy";

          keyboard = {
            layout = "pl";
          };

          sessionVariables = {
            EDITOR = "vim";
          };
        };
      };
    };
  };
}
