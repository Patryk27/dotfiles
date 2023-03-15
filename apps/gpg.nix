{ pkgs, ... }: {
  environment = {
    systemPackages = with pkgs; [
      gnupg
    ];
  };

  programs = {
    gnupg = {
      agent.enable = true;
      agent.enableSSHSupport = true;
    };
  };
}
