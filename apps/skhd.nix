{ pkgs, ... }: {
  services = {
    skhd = {
      enable = true;

      # https://github.com/koekeishiya/skhd/issues/42
      package = pkgs.writeScriptBin "skhd" ''
        export SHELL=/bin/bash
        exec ${pkgs.skhd}/bin/skhd "$@"
      '';

      skhdConfig = ''
        alt - 0x21 : yabai -m space --focus prev
        alt - 0x1E : yabai -m space --focus next
      '';
    };
  };
}
