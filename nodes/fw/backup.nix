{ pkgs, ... }:
{
  home-manager.users.pwy = {
    home = {
      packages = [
        (pkgs.writeShellScriptBin "borg.archive.backup-pfw" ''
          BORG_REPO="ssh://archive/home/pwy/backup-pfw" \
          BORG_PASSPHRASE="nastily-simile-dingy" \
          ${pkgs.borgbackup}/bin/borg $@
        '')

        (pkgs.writeShellScriptBin "borg.glacier.gateway" ''
          BORG_REPO="ssh://glacier/home/gateway" \
          BORG_PASSPHRASE="dolphin-rejoicing-collide" \
          ${pkgs.borgbackup}/bin/borg $@
        '')
      ];
    };
  };

  systemd = {
    services = {
      backup = {
        script = ''
          export PATH="$PATH:${pkgs.borgbackup}/bin"
          export PATH="$PATH:${pkgs.openssh}/bin"
          export PATH="$PATH:${pkgs.rsync}/bin"
          export PATH="$PATH:${pkgs.util-linux}/bin"
          export PATH="$PATH:${pkgs.zfs}/bin"
          export PATH="$PATH:/run/wrappers/bin"

          export BORG_REPO="ssh://archive/home/pwy/backup-pfw"
          export BORG_PASSPHRASE="nastily-simile-dingy"

          ${./backup/main.sh}
        '';
      };
    };

    timers = {
      backup = {
        wantedBy = [ "timers.target" ];
        partOf = [ "backup.service" ];

        timerConfig = {
          OnCalendar = "18:00:00";
        };
      };
    };
  };
}
