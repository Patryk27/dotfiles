{ pkgs, ... }: {
  environment = {
    variables = {
      # Required for emacs to recognize the agent
      SSH_AUTH_SOCK = "/run/user/1000/gnupg/S.gpg-agent.ssh";
    };
  };

  home-manager.users.pwy = {
    programs = {
      gpg = {
        enable = true;
      };
    };

    services = {
      gpg-agent = {
        enable = true;
        enableExtraSocket = true;
        pinentryFlavor = "qt";

        defaultCacheTtl = 14400;
        maxCacheTtl = 14400;

        enableSshSupport = true;
        defaultCacheTtlSsh = 14400;
        maxCacheTtlSsh = 14400;
      };
    };
  };
}
