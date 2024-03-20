#!/usr/bin/env bash

set -e

function do-help {
    echo "usage:"
    echo "  res mount <resource>"
    echo "  res umount <resource>"
    echo
    echo "resources:"
    echo "  - diary"
    echo "  - phone"

    exit 0
}

function do-mount {
    if [[ -z "$1" ]]; then
        do-help
    fi

    case "$1" in
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

function do-mount--diary {
    if [[ -d /private/diary ]]; then
        echo "error: resource already mounted"
        exit 1
    fi

    if ssh warp 'test -e /mnt/diary'; then
        echo "error: resource already mounted at the remote"
        exit 1
    fi

    echo "[+] Opening crypt"

    ssh warp -- \
        mkdir /mnt/diary

    ssh warp -- \
        gocryptfs /var/lib/storages/diary /mnt/diary

    echo "[+] Mounting"

    sudo mkdir /private/diary
    sudo chown pwy:staff /private/diary

    sshfs \
        -o reconnect,ServerAliveInterval=5,ServerAliveCountMax=1 \
        warp:/mnt/diary /private/diary
}

function do-umount--diary {
    if [[ ! -d /private/diary ]]; then
        echo "error: resource not mounted"
        exit 1
    fi

    echo "[+] Unmounting"

    umount /private/diary
    sudo rm -d /private/diary

    echo "[+] Closing crypt"

    ssh warp -- \
        umount /mnt/diary

    ssh warp -- \
        rm -d /mnt/diary
}

function do-mount--phone {
    if [[ -d /private/phone ]]; then
        echo "error: resource already mounted"
        exit 1
    fi

    echo "[+] Mounting"
    sudo mkdir /private/phone
    sudo chown pwy:staff /private/phone
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
    sudo rm -d /private/phone
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
