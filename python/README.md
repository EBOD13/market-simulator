# Python data pipeline

This directory contains the Python-side preprocessing for market-data sources.

## Workflow

1. Download a raw dataset file.
2. Run schema validation and normalization.
3. Export a cleaned dataset for the R modeling layer.

## Typical usage

```bash
python python/download_lobster_data.py --url "https://example.com/raw.csv" --output data/raw/raw_lobster.csv
python python/clean_lobster_data.py --input data/raw/raw_lobster.csv --output data/processed/lobster_clean.csv
python python/export_lobster_data.py --input data/processed/lobster_clean.csv --output data/processed/lobster_export.csv
```

## Notes

- The Python pipeline is intended for raw acquisition and normalization.
- The R scripts are intended for empirical modeling and simulation.
- The project currently includes a normalized book-update CSV and should eventually add true LOBSTER message files and their corresponding orderbook snapshots.
