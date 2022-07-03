{ pkgs, ... }: {
  imports = [
    ./home/git.nix
    ./home/gpg.nix
    ./home/lorri.nix
    ./home/programming.nix
    ./home/ssh.nix
    ./home/vim.nix
    ./home/zsh.nix
  ];

  environment = {
    systemPackages = [
      (pkgs.writeShellScriptBin "nm-current-ssid" ''
        nmcli -t -f name,device connection show --active | rg wlp0s20f3 | cut -d\: -f1
      '')
    ];
  };

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;

    users = {
      root = {
        home = {
          stateVersion = "18.09";
        };
      };

      pwy = {
        home = {
          stateVersion = "18.09";
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
