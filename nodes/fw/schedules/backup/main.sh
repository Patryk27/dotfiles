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
  echo "error: temporary directory already exists: ${tmp}"
  exit 1
fi

mkdir "${tmp}"

# ---

for dataset in "${DATASETS[@]}"; do
  echo
  echo "synchronizing: ${dataset}"

  dataset_id="${dataset//\//-}"
  dataset_snapshot="${dataset}@backup"

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
