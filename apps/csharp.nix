{ pkgs, ... }: {
  home-manager.users.PWY = {
    home = {
      packages = with pkgs; [
        (with dotnetCorePackages; combinePackages [
          sdk_6_0
          sdk_7_0
          sdk_8_0
        ])

        csharp-ls
      ];
    };
  };
}
