export BORG_PASSPHRASE=$(cat /run/secrets/backup:passphrase)

case "$1" in
    "" | "-h" | "help")
        echo "usage:"
        echo "  ./backup create"
        echo "  ./backup mount"
        echo "  ./backup umount"
        exit 1
        ;;

    "create")
        echo "[+] Creating backup"

        borg create \
            --progress \
            --stats \
            --exclude-caches \
            --exclude "/home/*/.cache" \
            --exclude "/home/*/.cargo" \
            --exclude "/home/*/.config/Slack" \
            --exclude "/home/*/.config/Vivaldi" \
            --exclude "/home/*/.config/google-chrome" \
            --exclude "/home/*/.local/share/containers" \
            --exclude "/home/*/.local/share/Trash" \
            --exclude "/home/*/.npm" \
            --exclude "/home/*/.rustup" \
            --exclude "/home/*/.steam" \
            --exclude "/home/*/.vagrant.d" \
            --exclude "/home/*/keybase" \
            ::{now} \
            ~ \
            /camera
        ;;

    "mount")
        echo "[+] Mounting"

        if [[ ! -d /mnt/backup ]]; then
            sudo mkdir /mnt/backup
            sudo chown "$USER" /mnt/backup
        fi

        borg mount "$BORG_REPO" /mnt/backup
        ;;

    "umount")
        echo "[+] Unmounting"
        borg umount /mnt/backup
        ;;

    *)
        echo "error: unknown command"
        exit 1
esac
