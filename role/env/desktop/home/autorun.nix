{ pkgs, ... }:

let
  autorun = pkgs.writeShellScript "autorun" ''
    ${pkgs.sway}/bin/swaymsg "workspace 1; exec kitty"
    ${pkgs.sway}/bin/swaymsg "workspace 2; exec firefox"
    ${pkgs.sway}/bin/swaymsg "workspace 3; exec kitty"
    ${pkgs.sway}/bin/swaymsg "workspace 4; exec thunderbird"
    ${pkgs.sway}/bin/swaymsg "workspace 8; exec keybase-gui"
    ${pkgs.sway}/bin/swaymsg "workspace 2"
  '';

in
{
  home-manager.users.pwy = {
    systemd = {
      user = {
        services = {
          autorun = {
            Unit = {
              PartOf = [ "graphical-session.target" ];
              After = [ "graphical-session.target" ];
            };

            Service = {
              Type = "simple";
              ExecStart = toString autorun;
              Restart = "no";
            };

            Install = {
              WantedBy = [ "graphical-session.target" ];
            };
          };
        };
      };
    };
  };
}
