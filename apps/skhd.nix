{ ... }: {
  services = {
    skhd = {
      enable = true;

      skhdConfig = ''
        alt - 0x21 : yabai -m space --focus prev
        alt - 0x1E : yabai -m space --focus next
      '';
    };
  };
}
