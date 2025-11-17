#!/usr/bin/env python3
"""
Process CSV files to keep only the first 8 columns.
Handles large files efficiently row-by-row to minimize memory usage.
Uses only Python standard library (no external dependencies required).
"""

import os
import sys
import csv
from pathlib import Path

def process_csv_file(file_path, num_columns=8):
    """
    Process a single CSV file to keep only the first N columns.
    Processes row-by-row to handle large files efficiently.

    Args:
        file_path: Path to the CSV file
        num_columns: Number of columns to keep (default: 8)
    """
    print(f"Processing: {file_path}")

    temp_file = f"{file_path}.tmp"

    try:
        # Get file size for progress indication
        file_size = os.path.getsize(file_path)
        print(f"  File size: {file_size / (1024*1024):.2f} MB")

        total_rows = 0

        # Process file row by row
        with open(file_path, 'r', newline='', encoding='utf-8') as infile, \
             open(temp_file, 'w', newline='', encoding='utf-8') as outfile:

            reader = csv.reader(infile)
            writer = csv.writer(outfile)

            for row in reader:
                # Keep only first 8 columns
                trimmed_row = row[:num_columns]
                writer.writerow(trimmed_row)

                total_rows += 1
                if total_rows % 10000 == 0:
                    print(f"  Processed {total_rows} rows...", end='\r')

        print(f"  Processed {total_rows} rows... Done!")

        # Replace original file with processed file
        os.replace(temp_file, file_path)
        print(f"  ✓ Successfully updated {file_path}")

    except Exception as e:
        print(f"  ✗ Error processing {file_path}: {e}")
        # Clean up temp file if it exists
        if os.path.exists(temp_file):
            os.remove(temp_file)
        raise

def main():
    # Path to the xdata directory
    xdata_dir = Path("dataverse_files/xdata")

    if not xdata_dir.exists():
        print(f"Error: Directory '{xdata_dir}' does not exist.")
        print("Please ensure the CSV files are in the correct location.")
        sys.exit(1)

    # Find all CSV files
    csv_files = sorted(xdata_dir.glob("*.csv"))

    if not csv_files:
        print(f"No CSV files found in '{xdata_dir}'")
        sys.exit(1)

    print(f"Found {len(csv_files)} CSV file(s) to process")
    print("=" * 60)

    # Process each file one by one
    for i, csv_file in enumerate(csv_files, 1):
        print(f"\n[{i}/{len(csv_files)}] {csv_file.name}")
        try:
            process_csv_file(csv_file, num_columns=8, chunk_size=10000)
        except Exception as e:
            print(f"Failed to process {csv_file.name}, continuing with next file...")
            continue

    print("\n" + "=" * 60)
    print("Processing complete!")

if __name__ == "__main__":
    main()
