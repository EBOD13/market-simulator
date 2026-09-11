#!/usr/bin/env python3
"""Run the full Python-side LOBSTER normalization pipeline for a raw input file.

This script is a convenience wrapper around the normalization logic in
clean_lobster_data.py. It is intended to be the one command used by the project
when a raw dataset has been located and needs to be transformed into a clean,
analysis-ready dataset for the R modeling layer.
"""

from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd

from clean_lobster_data import normalize_dataframe, read_raw_csv
from lobster_orderbook import compute_book_features, load_orderbook


def main() -> None:
    parser = argparse.ArgumentParser(description="Normalize a raw LOBSTER-style dataset.")
    parser.add_argument("--input", required=True, help="Input message CSV file path")
    parser.add_argument(
        "--output",
        default="data/processed/lobster_normalized.csv",
        help="Output CSV path for the processed message file",
    )
    parser.add_argument(
        "--orderbook",
        help="Optional path to the row-aligned LOBSTER orderbook CSV. When given, "
        "book-state features (spread, mid price, imbalance, depth) are joined "
        "onto each event by row position and written to --book-output.",
    )
    parser.add_argument(
        "--book-output",
        default="data/processed/lobster_with_book.csv",
        help="Output CSV path for the message+orderbook joined dataset",
    )
    args = parser.parse_args()

    in_path = Path(args.input)
    out_path = Path(args.output)
    out_path.parent.mkdir(parents=True, exist_ok=True)

    if not in_path.exists():
        raise FileNotFoundError(f"Input file not found: {in_path}")

    df, is_lobster_message = read_raw_csv(in_path)
    cleaned = normalize_dataframe(df, rescale_fixed_point_price=is_lobster_message)

    cleaned.to_csv(out_path, index=False)
    cleaned.to_parquet(out_path.with_suffix(".parquet"), index=False)

    print(f"Input rows: {len(df)}")
    print(f"Output rows: {len(cleaned)}")
    print(f"CSV output: {out_path}")
    print(f"Parquet output: {out_path.with_suffix('.parquet')}")
    print(cleaned["event_type"].value_counts(dropna=False).to_string())

    if args.orderbook:
        ob_path = Path(args.orderbook)
        if not ob_path.exists():
            raise FileNotFoundError(f"Orderbook file not found: {ob_path}")

        orderbook = load_orderbook(ob_path)
        if len(orderbook) != len(df):
            raise ValueError(
                f"Orderbook has {len(orderbook)} rows but the message file has "
                f"{len(df)} rows — LOBSTER requires a 1:1 row alignment between "
                "them, so this pair does not match."
            )

        # `cleaned` keeps the original row positions as its index (normalization
        # only filters rows, it never reindexes), so use it to pull the matching
        # orderbook snapshot for each surviving event.
        aligned_book = compute_book_features(orderbook.loc[cleaned.index])

        book_out_path = Path(args.book_output)
        book_out_path.parent.mkdir(parents=True, exist_ok=True)
        merged = pd.concat(
            [cleaned.reset_index(drop=True), aligned_book.reset_index(drop=True)], axis=1
        )
        merged.to_csv(book_out_path, index=False)
        merged.to_parquet(book_out_path.with_suffix(".parquet"), index=False)

        print(f"Book-joined output rows: {len(merged)}")
        print(f"Book-joined CSV output: {book_out_path}")
        print(f"Book-joined Parquet output: {book_out_path.with_suffix('.parquet')}")
        print(merged[["spread", "mid_price", "imbalance_l1", "depth_imbalance"]].describe().to_string())


if __name__ == "__main__":
    main()
