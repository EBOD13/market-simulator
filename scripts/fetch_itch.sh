#!/usr/bin/env bash
# scripts/fetch_itch.sh
#
# Download and decompress one day of Nasdaq TotalView-ITCH 5.0 session data
# from https://emi.nasdaq.com/ITCH/ (plain HTTPS, no auth required).
#
# The output is the raw decompressed ITCH binary session file. This script
# does not decode it to CSV -- that is tools/itch/itch_to_csv.cpp in the Leka
# repo. Run that tool against the file this script produces.
#
# Usage:
#   scripts/fetch_itch.sh DATE [VENUE] [OUT_DIR]
#
#   DATE     Session date as YYYYMMDD, e.g. 20190730
#   VENUE    bx (default) | nasdaq | psx
#   OUT_DIR  Destination directory (default: data/raw/itch)
#
# Examples:
#   scripts/fetch_itch.sh 20190730            # Nasdaq BX, ~0.36-1.5 GB gzipped
#   scripts/fetch_itch.sh 20190730 nasdaq     # main Nasdaq, ~3.3-16.7 GB gzipped
#
# Flags:
#   --force     Re-download even if the output files already exist
#   --keep-gz   Keep the downloaded .gz after decompressing (default: kept anyway;
#               this flag is a no-op retained for interface stability)
#   -h, --help  Show this help

set -euo pipefail

FORCE=0
POSITIONAL=()
for arg in "$@"; do
  case "$arg" in
    --force) FORCE=1 ;;
    --keep-gz) : ;;
    -h|--help)
      sed -n '2,26p' "$0"
      exit 0
      ;;
    *) POSITIONAL+=("$arg") ;;
  esac
done
set -- "${POSITIONAL[@]+"${POSITIONAL[@]}"}"

DATE="${1:-}"
VENUE="${2:-bx}"
OUT_DIR="${3:-data/raw/itch}"

if [[ -z "$DATE" ]]; then
  echo "Usage: scripts/fetch_itch.sh DATE [VENUE] [OUT_DIR]" >&2
  echo "  DATE as YYYYMMDD, e.g. 20190730. VENUE: bx (default) | nasdaq | psx." >&2
  exit 1
fi

if ! [[ "$DATE" =~ ^[0-9]{8}$ ]]; then
  echo "Error: DATE must be YYYYMMDD (got '$DATE')." >&2
  exit 1
fi

BASE_URL="https://emi.nasdaq.com/ITCH"
YEAR="${DATE:0:4}"
MONTH="${DATE:4:2}"
DAY="${DATE:6:2}"

case "$VENUE" in
  bx)
    SUBDIR="Nasdaq BX ITCH"
    FILENAME="${DATE}.BX_ITCH_50.gz"
    ;;
  psx)
    SUBDIR="Nasdaq PSX ITCH"
    FILENAME="${DATE}.PSX_ITCH_50.gz"
    ;;
  nasdaq)
    SUBDIR="Nasdaq ITCH"
    # The main Nasdaq venue uses MMDDYYYY, unlike BX/PSX's YYYYMMDD.
    FILENAME="${MONTH}${DAY}${YEAR}.NASDAQ_ITCH50.gz"
    ;;
  *)
    echo "Error: unknown venue '$VENUE' (expected bx, nasdaq, or psx)." >&2
    exit 1
    ;;
esac

# URL-encode the one space in the subdirectory name.
ENCODED_SUBDIR="${SUBDIR// /%20}"
URL="${BASE_URL}/${ENCODED_SUBDIR}/${FILENAME}"

mkdir -p "$OUT_DIR"
GZ_PATH="${OUT_DIR}/${FILENAME}"
RAW_PATH="${OUT_DIR}/${FILENAME%.gz}"

if [[ -f "$RAW_PATH" && "$FORCE" -eq 0 ]]; then
  echo "Already have decompressed file: $RAW_PATH (use --force to re-fetch)"
  exit 0
fi

echo "Fetching: $URL"

REMOTE_SIZE="$(curl -sSL -I --max-time 30 "$URL" | tr -d '\r' | awk -F': ' 'tolower($1)=="content-length"{print $2}' | tail -1)"
if [[ -z "$REMOTE_SIZE" ]]; then
  echo "Error: could not reach $URL (check DATE/VENUE)." >&2
  exit 1
fi
echo "Remote size: $(( REMOTE_SIZE / 1024 / 1024 )) MB gzipped"

# ITCH's repetitive fixed-size binary messages compress well; assume up to
# ~8x expansion as a conservative headroom check, not a measured constant.
NEEDED_BYTES=$(( REMOTE_SIZE * 9 ))
AVAILABLE_BYTES=$(df -Pk "$OUT_DIR" | awk 'NR==2 {print $4 * 1024}')
if [[ -n "$AVAILABLE_BYTES" && "$AVAILABLE_BYTES" -lt "$NEEDED_BYTES" ]]; then
  echo "Error: only $(( AVAILABLE_BYTES / 1024 / 1024 )) MB free in $OUT_DIR;" >&2
  echo "  want headroom for a gzipped file this size (up to ~$(( NEEDED_BYTES / 1024 / 1024 )) MB decompressed, worst case)." >&2
  echo "  Re-run with a roomier OUT_DIR, or pass --force once you've freed space." >&2
  exit 1
fi

if [[ "$FORCE" -eq 1 || ! -f "$GZ_PATH" || "$(stat -f%z "$GZ_PATH" 2>/dev/null || stat -c%s "$GZ_PATH" 2>/dev/null)" != "$REMOTE_SIZE" ]]; then
  curl -sSL --fail --retry 3 -C - -o "$GZ_PATH" "$URL"
else
  echo "Already have complete download: $GZ_PATH"
fi

MD5_URL="${URL}.md5sum"
MD5_HTTP_CODE="$(curl -sSL -o /tmp/itch_md5sum.$$ -w '%{http_code}' --max-time 20 "$MD5_URL" || echo "000")"
if [[ "$MD5_HTTP_CODE" == "200" ]]; then
  EXPECTED_MD5="$(awk '{print $1}' /tmp/itch_md5sum.$$)"
  ACTUAL_MD5="$(md5 -q "$GZ_PATH" 2>/dev/null || md5sum "$GZ_PATH" | awk '{print $1}')"
  if [[ "$EXPECTED_MD5" == "$ACTUAL_MD5" ]]; then
    echo "Checksum OK: $ACTUAL_MD5"
  else
    echo "Error: checksum mismatch (expected $EXPECTED_MD5, got $ACTUAL_MD5)." >&2
    rm -f /tmp/itch_md5sum.$$
    exit 1
  fi
else
  echo "Note: no .md5sum available for this file (HTTP $MD5_HTTP_CODE) -- skipping checksum verification."
fi
rm -f /tmp/itch_md5sum.$$

echo "Decompressing to: $RAW_PATH"
gzip -dc "$GZ_PATH" > "$RAW_PATH.partial"
mv "$RAW_PATH.partial" "$RAW_PATH"

echo "Done."
echo "  Compressed:   $GZ_PATH"
echo "  Decompressed: $RAW_PATH"
echo "Next: decode it with the Leka repo's tools/itch/itch_to_csv.cpp."
