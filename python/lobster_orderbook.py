#!/usr/bin/env python3
"""Load a LOBSTER orderbook CSV and derive book-state features.

LOBSTER orderbook files have no header and no timestamp column: row n is the
book snapshot immediately after row n of the corresponding message file, with
columns [ask_price_1, ask_size_1, bid_price_1, bid_size_1, ...] repeated for
each depth level. This module labels those columns, rescales LOBSTER's
fixed-point prices, and computes the spread/imbalance/depth features needed
to model event probabilities conditional on book state.
"""

from __future__ import annotations

from pathlib import Path

import pandas as pd

# LOBSTER's sentinel for a missing level (book has fewer than N levels).
_SENTINEL_MAGNITUDE = 1e9


def orderbook_column_names(levels: int) -> list[str]:
    names = []
    for level in range(1, levels + 1):
        names += [
            f"ask_price_{level}",
            f"ask_size_{level}",
            f"bid_price_{level}",
            f"bid_size_{level}",
        ]
    return names


def load_orderbook(path: Path) -> pd.DataFrame:
    """Read a headerless LOBSTER orderbook CSV, labeled and price-rescaled."""
    with Path(path).open() as f:
        first_line = f.readline()
    num_fields = len(first_line.strip().split(","))

    if num_fields % 4 != 0:
        raise ValueError(
            f"Orderbook file has {num_fields} columns; expected a multiple of 4 "
            "(ask_price, ask_size, bid_price, bid_size per level)."
        )
    levels = num_fields // 4

    df = pd.read_csv(path, header=None, names=orderbook_column_names(levels))

    price_cols = [c for c in df.columns if "price" in c]
    size_cols = [c for c in df.columns if "size" in c]

    for col in price_cols:
        df[col] = pd.to_numeric(df[col], errors="coerce")
        missing = df[col].abs() >= _SENTINEL_MAGNITUDE
        df.loc[missing, col] = pd.NA
        df[col] = df[col] / 10000.0

    for col in size_cols:
        df[col] = pd.to_numeric(df[col], errors="coerce")
        df.loc[df[col].abs() >= _SENTINEL_MAGNITUDE, col] = pd.NA

    df.attrs["levels"] = levels
    return df


def compute_book_features(df: pd.DataFrame, levels: int | None = None) -> pd.DataFrame:
    """Add spread/mid/imbalance/depth columns derived from the labeled book."""
    if levels is None:
        levels = df.attrs.get("levels")
    if levels is None:
        levels = max(int(c.rsplit("_", 1)[1]) for c in df.columns if c.startswith("ask_price_"))

    out = df.copy()
    out["spread"] = out["ask_price_1"] - out["bid_price_1"]
    out["mid_price"] = (out["ask_price_1"] + out["bid_price_1"]) / 2
    out["imbalance_l1"] = (out["bid_size_1"] - out["ask_size_1"]) / (
        out["bid_size_1"] + out["ask_size_1"]
    )

    bid_size_cols = [f"bid_size_{i}" for i in range(1, levels + 1)]
    ask_size_cols = [f"ask_size_{i}" for i in range(1, levels + 1)]
    out["bid_depth"] = out[bid_size_cols].sum(axis=1, min_count=1)
    out["ask_depth"] = out[ask_size_cols].sum(axis=1, min_count=1)
    out["depth_imbalance"] = (out["bid_depth"] - out["ask_depth"]) / (
        out["bid_depth"] + out["ask_depth"]
    )

    return out
