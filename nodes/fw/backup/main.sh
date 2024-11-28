set -e

DATASETS=(
  "rpool"
  "rpool/home"
  "rpool/home/diary"
  "rpool/home/x"
)

# ------------------------------------------------------------------------------

echo "checking connection"
borg info

# ---

echo "preparing temporary directory"
echo

if [[ -d /run/backup ]]; then
  echo "warn: temporary directory already exists, cleaning up"

  if mountpoint -q /run/backup; then
    umount /run/backup
  fi

  rm -d /run/backup
fi

mkdir /run/backup

# ---

for dataset in "${DATASETS[@]}"; do
  echo
  echo "synchronizing: ${dataset}"

  dataset_id="${dataset//\//-}"
  dataset_snapshot="${dataset}@backup"

  if zfs list -t snapshot -o name -H "${dataset_snapshot}" &> /dev/null; then
    echo "warn: temporary snapshot already exists, cleaning up"
    zfs destroy "${dataset_snapshot}"
  fi

  zfs snapshot "${dataset_snapshot}"
  mount -t zfs "${dataset_snapshot}" /run/backup
  cd /run/backup

  borg create \
    --stats \
    --compression zstd \
    --exclude-caches \
    ::"${dataset_id}|"{now} \
    .

  borg prune \
    -P "${dataset_id}|" \
    -v \
    --list \
    --keep-daily 8 \
    --keep-weekly 6 \
    --keep-monthly 4 \
    --keep-yearly 1

  cd -
  umount /run/backup
  zfs destroy "${dataset_snapshot}"
done

# ---

echo
echo "wrapping up"

borg compact \
  --verbose \
  --progress

rm -d /run/backup
