{ config, pkgs, ... }: {
  imports = [
    ./default/home.nix
    ./default/nix.nix
    ./default/virtualisation.nix
  ];

  console = {
    keyMap = "pl";
  };

  environment = {
    systemPackages = with pkgs; [
      aria
      bmon
      curl
      debianutils
      dnsutils
      fd
      ffmpeg-full
      file
      fzf
      htop
      i7z
      iotop
      libnotify
      ncat
      ncdu
      openvpn
      powertop
      psmisc
      ripgrep
      rsync
      sshfs
      tree
      unrar
      unzip
      wget
      zsh
    ];
  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  networking = {
    networkmanager = {
      enable = true;
    };

    firewall = {
      enable = true;
      allowPing = true;
      allowedTCPPorts = [ 2000 2001 2002 ];
    };
  };

  powerManagement = {
    enable = true;
  };

  security = {
    sudo = {
      wheelNeedsPassword = false;
    };
  };

  services = {
    getty = {
      autologinUser = "pwy";
    };

    openssh = {
      enable = true;
      passwordAuthentication = false;
      permitRootLogin = "no";
      ports = [ 33000 ];

      extraConfig = ''
        StreamLocalBindUnlink yes
      '';
    };
  };

  time = {
    timeZone = "Europe/Warsaw";
  };

  users = {
    defaultUserShell = pkgs.zsh;

    users = {
      pwy = {
        uid = 1000;
        home = "/home/pwy";
        extraGroups = [ "audio" "disk" "docker" "libvirtd" "lxd" "networkmanager" "video" "wheel" ];
        isNormalUser = true;
        createHome = true;

        openssh = {
          authorizedKeys = {
            keys = [
              "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDPn9SdTEI7JlofCoa+CanrDmjQjz4U+h9KHEqB+weNg+ZoNETTQki6NcsgmivwQ9jG+8n0hIhq0viLoE34amUOo+Wa3CWsb1cFTQ9nCfb7yvfCawRP+lcaPw1tKBfkSmph2TyExGb7t8msNW6nB8/oaRgekKQf/Eg+Ib+UzSl7F7XPEwsQgAJhLF681Ye2tlEMT2r/Wx9gWf0szPHENKYeqJt+VrJv+2+UbDUpBYSPYTpE5Hy5BzLLHsARGnMuRs8SHbE+bNhN77jWf0rHbtXKTPVBQpVsCYVBwYHcmaRtygEzA2UEkjqQejN3IIMB0ZnnTsgwacQo7+BFSbHQYxUXtDBuJOUppRI1cyCBrg1gmqeBx5tP/tdh8yf1yVyYN4WV4m7h2rJZhuzexKMVEHjnXGtsed5xvvNeNhPkLyxUo+yKYmN47xpb57OjVrF21sfcsrPzlJbQwhwm/MuVN3B7oytfU281nrep+AhusUuRKBSFhhdSeEZEyK2tBdhKYyfT9xOl1VuxyloPfAlyY37FuwpGwaGx6Fc/HROg+0zbZfsURyaj4I938Z3YQ2/By1fzWXwKCNJubbR4ybuHC/3qZ/jtewOGpKVj+bc+Z1ajmUo7swZoDeEzZ+pWTc15mdj6wlSojkRpmbZ6S8wt0nLPFHM7sCf3fi7fK7npmjcNiw== cardno:000614320925"
            ];
          };
        };
      };
    };
  };
}
