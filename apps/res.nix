{ pkgs, ... }: {
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "res" ''
      set -e

      export PATH="$PATH:${pkgs.gocryptfs}/bin"
      export PATH="$PATH:${pkgs.ifuse}/bin"

      ${./res/main.sh} "$@"
    '')
  ];
}
