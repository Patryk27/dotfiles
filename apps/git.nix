{ ... }: {
  home-manager.users.PWY = {
    programs = {
      git = {
        enable = true;
        userName = "Patryk Wychowaniec";
        userEmail = "pwychowaniec@pm.me";

        delta = {
          enable = true;

          options = {
            dark = true;
          };
        };

        signing = {
          key = "196ABFEC6A1DD1EC7594F8D1F62547D075E09767";
          signByDefault = true;
        };

        extraConfig = {
          core = {
            commentChar = ";";
          };

          fetch = {
            prune = true;
          };

          merge = {
            conflictStyle = "diff3";
          };

          pull = {
            rebase = true;
          };

          safe = {
            directory = "*";
          };
        };
      };
    };
  };
}
