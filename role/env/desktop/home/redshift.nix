{ pkgs, ... }: {
  home-manager.users.pwy = {
    services = {
      redshift = {
        enable = true;
        package = pkgs.redshift-wlr;
        latitude = "51.107";
        longitude = "17.038";
      };
    };
  };
}
