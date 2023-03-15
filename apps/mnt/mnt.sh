#!/usr/bin/env bash

set -e

function do-help {
    echo "usage:"
    echo "  mnt mount <resource>"
    echo "  mnt umount <resource>"
    echo
    echo "resources:"
    echo "  - backup"
    echo "  - cloud"
    echo "  - diary"
    echo "  - phone"

    exit 0
}

function do-mount {
    if [[ -z "$1" ]]; then
        do-help
    fi

    case "$1" in
        "backup")
            do-mount--backup
            ;;

        "cloud")
            do-mount--cloud
            ;;

        "diary")
            do-mount--diary
            ;;

        "phone")
            do-mount--phone
            ;;

        *)
            echo "error: unknown resource: $1"
            exit 1
    esac
}

function do-umount {
    if [[ -z "$1" ]]; then
        do-help
    fi

    case "$1" in
        "backup")
            do-umount--backup
            ;;

        "cloud")
            do-umount--cloud
            ;;

        "diary")
            do-umount--diary
            ;;

        "phone")
            do-umount--phone
            ;;

        *)
            echo "error: unknown resource: $1"
            exit 1
    esac
}

function do-mount--backup {
    if [[ -d /private/backup ]]; then
        echo "error: resource already mounted"
        exit 1
    fi

    backup mount
}

function do-umount--backup {
    if [[ ! -d /private/backup ]]; then
        echo "error: resource not mounted"
        exit 1
    fi

    backup umount
    rm -d /private/backup
}

function do-mount--cloud {
    if [[ -d /private/cloud ]]; then
        echo "error: resource already mounted"
        exit 1
    fi

    echo "[+] Preparing mount point"
    mkdir /private/cloud

    echo "[+] Detecting sftp-server"
    sftp_server=$(
        ssh warp -- grep Subsystem /etc/ssh/sshd_config | cut -d' ' -f3
    )

    echo "[+] Mounting"
    sshfs \
        -o reconnect,ServerAliveInterval=5,ServerAliveCountMax=1 \
        -o sftp_server="sudo -u '#999' -g '#999' ${sftp_server}" \
        warp:/var/lib/nixos-containers/nextcloud/var/lib/nextcloud/data /private/cloud
}

function do-umount--cloud {
    if [[ ! -d /private/cloud ]]; then
        echo "error: resource not mounted"
        exit 1
    fi

    echo "[+] Unmounting"
    sudo umount /private/cloud
    rm -d /private/cloud
}

function do-mount--diary {
    if [[ -d /private/diary ]]; then
        echo "error: resource already mounted"
        exit 1
    fi

    echo "[+] Preparing mount point"

    sudo mkdir /private/diary
    sudo chown pwy:staff /private/diary

    sudo mkdir /private/diary.data
    sudo chown pwy:staff /private/diary.data

    echo "[+] Mounting"
    sshfs \
        -o reconnect,ServerAliveInterval=5,ServerAliveCountMax=1 \
        warp:/var/lib/storages/diary /private/diary.data

    echo "[+] Opening crypt"
    gocryptfs /private/diary.data /private/diary
}

function do-umount--diary {
    if [[ ! -d /private/diary ]]; then
        echo "error: resource not mounted"
        exit 1
    fi

    echo "[+] Closing crypt"
    umount /private/diary

    echo "[+] Unmounting"
    umount /private/diary.data

    echo "[+] Cleaning-up"
    rm -d /private/diary
    rm -d /private/diary.data
}

function do-mount--phone {
    if [[ -d /private/phone ]]; then
        echo "error: resource already mounted"
        exit 1
    fi

    echo "[+] Mounting"
    mkdir /private/phone
    ifuse /private/phone
}

function do-umount--phone {
    if [[ ! -d /private/phone ]]; then
        echo "error: resource not mounted"
        exit 1
    fi

    echo "[+] Unmounting"
    umount /private/phone

    echo "[+] Cleaning-up"
    rm -d /private/phone
}

if [[ -z "$1" ]]; then
    do-help
fi

case "$1" in
    "mount")
        shift && do-mount "$@"
        ;;

    "umount")
        shift && do-umount "$@"
        ;;

    *)
        echo "error: unknown command: ${1}"
        exit 1
esac
