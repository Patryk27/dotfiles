{ pkgs, ... }: {
  environment = {
    sessionVariables = {
      MOZ_ENABLE_WAYLAND = "1";
      XDG_CURRENT_DESKTOP = "sway";
      XDG_SESSION_TYPE = "wayland";
    };

    systemPackages = with pkgs; [
      breeze-icons
      kde-gtk-config
      plasma5Packages.ark
      plasma5Packages.dolphin
      plasma5Packages.dolphin-plugins
      plasma5Packages.okular
      plasma5Packages.kconfig
      plasma5Packages.kconfigwidgets
      lxappearance
      pavucontrol
    ];
  };
}
