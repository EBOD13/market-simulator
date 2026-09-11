#!/usr/bin/env python3
"""Export a cleaned LOBSTER dataset in the format requested by the R analysis layer.

This script is intentionally a thin wrapper: it validates the processed dataset and
writes a CSV and Parquet copy so the R modeling pipeline can consume a stable file.
"""

from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd


def main() -> None:
    parser = argparse.ArgumentParser(description="Export a cleaned LOBSTER dataset.")
    parser.add_argument("--input", required=True, help="Path to the cleaned/normalized CSV file.")
    parser.add_argument(
        "--output",
        default="data/processed/lobster_export.csv",
        help="Destination path for the exported file.",
    )
    args = parser.parse_args()

    in_path = Path(args.input)
    out_path = Path(args.output)
    out_path.parent.mkdir(parents=True, exist_ok=True)

    df = pd.read_csv(in_path)
    if "event_time_ns" not in df.columns:
        raise ValueError("The input file does not contain event_time_ns. Run clean_lobster_data.py first.")

    if out_path.suffix.lower() == ".parquet":
        df.to_parquet(out_path, index=False)
    else:
        df.to_csv(out_path, index=False)
        parquet_path = out_path.with_suffix(".parquet")
        df.to_parquet(parquet_path, index=False)

    print(f"Exported rows: {len(df)}")
    print(f"Output file: {out_path}")


if __name__ == "__main__":
    main()
