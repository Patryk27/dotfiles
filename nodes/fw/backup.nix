{ config, pkgs, ... }:
{
  age = {
    secrets = {
      fw-backup.file = ../../secrets/fw-backup;
      gateway-backup.file = ../../secrets/gateway-backup;
      warp-backup.file = ../../secrets/warp-backup;
    };
  };

  home-manager.users.pwy = {
    home = {
      packages = [
        (pkgs.writeShellScriptBin "borg.archive.fw" ''
          BORG_REPO="ssh://archive/home/pwy/backup-pfw" \
          BORG_PASSPHRASE="$(sudo cat ${config.age.secrets.fw-backup.path})" \
          ${pkgs.borgbackup}/bin/borg $@
        '')

        (pkgs.writeShellScriptBin "borg.glacier.gateway" ''
          BORG_REPO="ssh://glacier/home/gateway" \
          BORG_PASSPHRASE="$(sudo cat ${config.age.secrets.gateway-backup.path})" \
          ${pkgs.borgbackup}/bin/borg $@
        '')

        (pkgs.writeShellScriptBin "borg.glacier.warp" ''
          BORG_REPO="ssh://glacier/home/warp" \
          BORG_PASSPHRASE="$(sudo cat ${config.age.secrets.warp-backup.path})" \
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
