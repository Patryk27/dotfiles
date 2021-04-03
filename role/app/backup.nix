{ pkgs, repo }: pkgs.writeShellScriptBin "backup" ''
  set -e

  export PATH="$PATH:${pkgs.borgbackup}/bin"
  export BORG_REPO="${repo}"

  ${./backup/backup.sh} "$@"
''
