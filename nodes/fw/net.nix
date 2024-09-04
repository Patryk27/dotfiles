{ pkgs, ... }: {
  environment = {
    systemPackages = with pkgs; [
      aria
      bmon
      curl
      nmap
      rsync
      snx-rs
      wget
      wrk
    ];
  };

  networking = {
    firewall = {
      allowedUDPPorts = [ 51820 ];
    };


    };
  };
}
