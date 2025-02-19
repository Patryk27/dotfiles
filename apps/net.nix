{ pkgs, ... }:
{
  home-manager.users.pwy = {
    home = {
      packages = with pkgs; [
        aria
        bmon
        curl
        dig
        nmap
        rsync
        wget
        wrk
      ];
    };
  };
}
