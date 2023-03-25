{ pkgs, ... }: {
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "mnt" ''
      set -e

      export PATH="$PATH:${pkgs.gocryptfs}/bin"
      export PATH="$PATH:${pkgs.ifuse}/bin"

      ${./mnt/mnt.sh} "$@"
    '')
  ];
}
