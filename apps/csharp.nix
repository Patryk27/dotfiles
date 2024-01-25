{ pkgs, ... }: {
  home-manager.users.PWY = {
    home = {
      packages = with pkgs; [
        dotnet-sdk
        omnisharp-roslyn
      ];
    };
  };
}
