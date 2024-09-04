set -e

DATASETS=(
  "rpool"
  "rpool/home"
  "rpool/q"
  "rpool/x"
)

tmp="/run/backup"

# ------------------------------------------------------------------------------

echo "checking connection"
borg info

# ---

echo "preparing temporary directory"
echo

if [[ -d "${tmp}" ]]; then
  echo "warn: temporary directory already exists, cleaning up"

  if mountpoint -q "${tmp}"; then
    umount "${tmp}"
  fi

  rm -d "${tmp}"
fi

mkdir "${tmp}"

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
  mount -t zfs "${dataset_snapshot}" "${tmp}"
  cd "${tmp}"

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
    --keep-daily 14 \
    --keep-weekly 8 \
    --keep-monthly 6 \
    --keep-yearly 2

  cd -
  umount "${tmp}"
  zfs destroy "${dataset_snapshot}"
done

# ---

echo
echo "wrapping up"

borg compact \
  --verbose \
  --progress

rm -d "${tmp}"
