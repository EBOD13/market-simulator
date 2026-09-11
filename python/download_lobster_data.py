#!/usr/bin/env python3
"""Download a single LOBSTER dataset file for a chosen instrument/configuration.

This script is intentionally simple: it downloads one raw CSV file from a URL and
stores it under data/raw/. It is a thin scaffold for the Python-first acquisition
step in the market-simulator pipeline.
"""

from __future__ import annotations

import argparse
from pathlib import Path
from urllib.request import urlretrieve


def download_file(url: str, destination: Path) -> Path:
    destination.parent.mkdir(parents=True, exist_ok=True)
    local_path, _ = urlretrieve(url, str(destination))
    return Path(local_path)


def main() -> None:
    parser = argparse.ArgumentParser(description="Download a single LOBSTER raw file.")
    parser.add_argument("--url", required=True, help="Download URL for the raw LOBSTER CSV file.")
    parser.add_argument(
        "--output",
        default="data/raw/lobster_raw.csv",
        help="Destination path for the downloaded file.",
    )
    args = parser.parse_args()

    out_path = Path(args.output)
    downloaded = download_file(args.url, out_path)
    print(f"Downloaded: {downloaded}")


if __name__ == "__main__":
    main()
