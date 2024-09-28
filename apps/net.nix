{ pkgs, user, ... }: {
  home-manager.users."${user}" = {
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
