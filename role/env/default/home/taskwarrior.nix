{ config, ... }: {
  home-manager.users.pwy = { pkgs, ... }: {
    programs = {
      taskwarrior = {
        enable = true;

        config = {
          recurrence = if config.networking.hostName == "lenovo" then "on" else "off";

          data = {
            location = "~/.task";
          };

          taskd = {
            server = "fort:53589";
            credentials = "org/patryk/66217992-a2c4-4462-86f0-770ec565793e";
            certificate = ./taskwarrior/client.cert;
            key = ./taskwarrior/client.key;
            ca = ./taskwarrior/ca.cert;
          };
        };
      };
    };
  };
}
