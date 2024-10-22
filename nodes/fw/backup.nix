{ pkgs, user, ... }: {
  home-manager.users."${user}" = {
    home = {
      packages = [
        (pkgs.writeShellScriptBin "backup-borg" ''
          BORG_REPO="ssh://archive/home/pwy/backup-fw" \
          BORG_PASSPHRASE="nastily-simile-dingy" \
          ${pkgs.borgbackup}/bin/borg $@
        '')

        (pkgs.writeShellScriptBin "gateway-borg" ''
          BORG_REPO="ssh://glacier/home/gateway" \
          BORG_PASSPHRASE="dolphin-rejoicing-collide" \
          ${pkgs.borgbackup}/bin/borg $@
        '')

        (pkgs.writeShellScriptBin "warp-borg" ''
          BORG_REPO="ssh://glacier/home/warp" \
          BORG_PASSPHRASE="pluck-cattishly-vertebrae" \
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
