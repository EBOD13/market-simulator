#!/usr/bin/env python3
"""Normalize a raw LOBSTER-style CSV into a processed dataset for R analysis.

This script intentionally focuses on the Python-side data engineering layer:
- validate columns
- normalize event codes
- normalize timestamps to integer nanoseconds
- clean obvious malformed rows
- write a processed Parquet/CSV dataset
"""

from __future__ import annotations

import argparse
from pathlib import Path

import pandas as pd


CANONICAL_COLUMNS = {
    "event_time": ["event_time", "time", "ts_event", "timestamp"],
    "order_id": ["order_id", "orderId", "oid"],
    "price": ["price", "px"],
    "size": ["size", "qty", "quantity"],
    "side": ["side", "direction", "side_type"],
    "event_type": ["event_type", "type", "event", "eventType"],
    "symbol": ["symbol", "ticker", "instrument"],
}


def pick_column(columns: list[str], aliases: list[str]) -> str | None:
    for alias in aliases:
        if alias in columns:
            return alias
    return None


def normalize_event_type(value: object) -> str:
    if pd.isna(value):
        return "UNKNOWN"

    s = str(value).strip().upper()
    mapping = {
        "1": "NEW",
        "2": "CANCEL",
        "3": "DELETE",
        "4": "EXECUTE_VISIBLE",
        "5": "EXECUTE_HIDDEN",
        "7": "HALT",
        "NEW": "NEW",
        "CANCEL": "CANCEL",
        "DELETE": "DELETE",
        "EXECUTE_VISIBLE": "EXECUTE_VISIBLE",
        "EXECUTE_HIDDEN": "EXECUTE_HIDDEN",
        "HALT": "HALT",
        "UPDATE": "UPDATE",
    }
    return mapping.get(s, s)


def normalize_side(value: object) -> str:
    if pd.isna(value):
        return "UNKNOWN"

    s = str(value).strip().upper()
    mapping = {
        "BUY": "BID",
        "B": "BID",
        "SELL": "ASK",
        "S": "ASK",
        "ASK": "ASK",
        "BID": "BID",
        "1": "BID",
        "2": "ASK",
        "-1": "ASK",
        "0": "UNKNOWN",
    }
    return mapping.get(s, s)


def normalize_lobster_event(value: object) -> str:
    """Map raw LOBSTER message codes to simulator-friendly event names."""
    if pd.isna(value):
        return "UNKNOWN"

    s = str(value).strip().upper()
    lobsters = {
        "1": "NEW",
        "2": "CANCEL",
        "3": "DELETE",
        "4": "EXECUTE_VISIBLE",
        "5": "EXECUTE_HIDDEN",
        "7": "HALT",
        "NEW": "NEW",
        "CANCEL": "CANCEL",
        "DELETE": "DELETE",
        "EXECUTE_VISIBLE": "EXECUTE_VISIBLE",
        "EXECUTE_HIDDEN": "EXECUTE_HIDDEN",
        "HALT": "HALT",
    }
    return lobsters.get(s, normalize_event_type(value))


def normalize_lobster_side(value: object) -> str:
    """Map raw LOBSTER direction values to ASK/BID."""
    if pd.isna(value):
        return "UNKNOWN"

    s = str(value).strip().upper()
    directions = {
        "1": "BID",
        "-1": "ASK",
        "BUY": "BID",
        "BID": "BID",
        "SELL": "ASK",
        "ASK": "ASK",
        "BUYER": "BID",
        "SELLER": "ASK",
    }
    return directions.get(s, normalize_side(value))


def normalize_order_type(value: object) -> str:
    if pd.isna(value):
        return "UNKNOWN"

    s = str(value).strip().upper()
    mapping = {
        "LIMIT": "LIMIT",
        "L": "LIMIT",
        "MARKET": "MARKET",
        "M": "MARKET",
        "0": "MARKET",
        "1": "LIMIT",
    }
    return mapping.get(s, "LIMIT")


def to_ns_timestamp(series: pd.Series) -> pd.Series:
    numeric = pd.to_numeric(series, errors="coerce")
    if numeric.notna().all():
        # LOBSTER message files store time as seconds after midnight, not a
        # calendar timestamp, so treat an all-numeric column as seconds.
        return (numeric * 1e9).round().astype("int64")

    parsed = pd.to_datetime(series, errors="coerce", utc=True)
    # Convert to integer nanoseconds, preserving the full precision.
    ns = parsed.view("int64")
    return ns


LOBSTER_MESSAGE_COLUMNS = ["event_time", "event_type", "order_id", "size", "price", "direction"]


def read_raw_csv(path: Path) -> tuple[pd.DataFrame, bool]:
    """Read a raw CSV, detecting LOBSTER's headerless message file format.

    LOBSTER message files ship with no header row and a fixed 6-column
    layout (time, type, order id, size, price, direction); everything else
    this pipeline consumes has a header row.
    """
    with path.open() as f:
        first_line = f.readline()
    first_field = first_line.split(",")[0].strip()

    is_headerless = True
    try:
        float(first_field)
    except ValueError:
        is_headerless = False

    if is_headerless and len(first_line.split(",")) == len(LOBSTER_MESSAGE_COLUMNS):
        return pd.read_csv(path, header=None, names=LOBSTER_MESSAGE_COLUMNS), True

    return pd.read_csv(path), False


def normalize_dataframe(df: pd.DataFrame, rescale_fixed_point_price: bool = False) -> pd.DataFrame:
    columns = list(df.columns)
    renamed = {}

    for canonical, aliases in CANONICAL_COLUMNS.items():
        match = pick_column(columns, aliases)
        if match:
            renamed[match] = canonical

    df = df.rename(columns=renamed)

    if "event_time" not in df.columns:
        raise ValueError("Input data does not contain a usable timestamp column.")

    required = ["event_time", "event_type"]
    for col in required:
        if col not in df.columns:
            raise ValueError(f"Missing required column: {col}")

    df["event_time_ns"] = to_ns_timestamp(df["event_time"])
    df["event_type_raw"] = df["event_type"]
    df["event_type"] = df["event_type"].map(normalize_lobster_event)

    if "side" in df.columns:
        df["side_raw"] = df["side"]
        df["side"] = df["side"].map(normalize_lobster_side)
    else:
        if "direction" in df.columns:
            df["side_raw"] = df["direction"]
            df["side"] = df["direction"].map(normalize_lobster_side)

    if "order_type" in df.columns:
        df["order_type_raw"] = df["order_type"]
        df["order_type"] = df["order_type"].map(normalize_order_type)
    elif "price" in df.columns:
        df["order_type_raw"] = df["price"]
        df["order_type"] = df["price"].map(lambda x: "MARKET" if pd.isna(x) or x == 0 else "LIMIT")

    if "price" in df.columns:
        df["price"] = pd.to_numeric(df["price"], errors="coerce")
        if rescale_fixed_point_price:
            # LOBSTER stores prices as fixed-point integers (price * 10^4).
            df["price"] = df["price"] / 10000.0

    if "size" in df.columns:
        df["size"] = pd.to_numeric(df["size"], errors="coerce")

    if "event_type" in df.columns:
        df = df[df["event_type"] != "UNKNOWN"]

    # Remove rows with invalid timestamps, and keep only valid market events.
    df = df.dropna(subset=["event_time_ns"])

    return df


def main() -> None:
    parser = argparse.ArgumentParser(description="Clean and normalize a LOBSTER raw CSV file.")
    parser.add_argument("--input", required=True, help="Path to the raw CSV file.")
    parser.add_argument(
        "--output",
        default="data/processed/lobster_clean.csv",
        help="Destination path for the processed output file.",
    )
    args = parser.parse_args()

    in_path = Path(args.input)
    out_path = Path(args.output)
    out_path.parent.mkdir(parents=True, exist_ok=True)

    df, is_lobster_message = read_raw_csv(in_path)
    cleaned = normalize_dataframe(df, rescale_fixed_point_price=is_lobster_message)

    if out_path.suffix.lower() == ".parquet":
        cleaned.to_parquet(out_path, index=False)
    else:
        cleaned.to_csv(out_path, index=False)

    if out_path.suffix.lower() != ".parquet":
        parquet_out = out_path.with_suffix(".parquet")
        cleaned.to_parquet(parquet_out, index=False)

    print(f"Input rows: {len(df)}")
    print(f"Output rows: {len(cleaned)}")
    print(cleaned["event_type"].value_counts(dropna=False).to_string())


if __name__ == "__main__":
    main()
