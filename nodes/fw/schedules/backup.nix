{ pkgs, ... }: {
  systemd = {
    services = {
      backup = {
        script = ''
          export PATH="$PATH:${pkgs.borgbackup}/bin"
          export PATH="$PATH:${pkgs.openssh}/bin"
          export PATH="$PATH:${pkgs.rsync}/bin"
          export PATH="$PATH:${pkgs.zfs}/bin"
          export PATH="$PATH:/run/wrappers/bin"

          export BORG_REPO="ssh://archive/home/pwy/backup-fw"
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
          OnCalendar = "12:00:00";
        };
      };
    };
  };
}
