{ pkgs, ... }:
{
  home-manager.users.pwy =
    { lib, ... }:
    {
      home = {
        packages = with pkgs; [
          sshfs
        ];
      };

      programs = {
        ssh = {
          enable = true;
          controlMaster = "auto";
          controlPersist = "10m";
          serverAliveInterval = 30;
        };
      };
    };
}
