{ pkgs, ... }: {
  environment.systemPackages = [
    (pkgs.writeShellScriptBin "mnt" ''
      set -e

      export PATH="$PATH:${pkgs.gocryptfs}/bin"

      ${./mnt/mnt.sh} "$@"
    '')
  ];
}
