{ pkgs, ... }: {
  home-manager.users.pwy = {
    services = {
      gammastep = {
        enable = true;
        latitude = "51.107";
        longitude = "17.038";
      };
    };
  };
}
