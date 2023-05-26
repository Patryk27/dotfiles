{ ... }: {
  services = {
    skhd = {
      enable = true;

      skhdConfig = ''
        alt - q : yabai -m space --focus prev
        alt - w : yabai -m space --focus next
      '';
    };
  };
}
