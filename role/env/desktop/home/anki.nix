{ pkgs, ... }:
let
  anki = pkgs.symlinkJoin {
    name = "anki";

    paths = with pkgs; [
      anki-bin
    ];

    buildInputs = with pkgs; [
      makeWrapper
    ];

    postBuild = ''
      wrapProgram $out/bin/anki \
        --set ANKI_WAYLAND 1
    '';
  };

in
{
  environment = {
    systemPackages = [
      anki
      pkgs.mpv
    ];
  };
}
