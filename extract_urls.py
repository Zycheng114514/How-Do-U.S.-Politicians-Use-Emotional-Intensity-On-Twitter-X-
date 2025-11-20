#!/usr/bin/env python3

import sys
from pathlib import Path
import pandas as pd


def extract_urls_to_txt(input_file, output_base, num_parts=4):

    print(f"Reading data from: {input_file}")
    print("=" * 60)

    # Read the CSV
    df = pd.read_csv(input_file)

    print(f"Total rows: {len(df):,}")
    print(f"Extracting 'urls' column...")

    # Extract the urls column and strip whitespace
    urls_list = []
    for url in df['urls']:
        if pd.isna(url):
            urls_list.append('')
        else:
            # Strip leading/trailing whitespace
            urls_list.append(str(url).strip())

    # Calculate rows per part
    total_rows = len(urls_list)
    rows_per_part = total_rows // num_parts
    remainder = total_rows % num_parts

    print(f"Splitting into {num_parts} parts (~{rows_per_part} rows each)")

    # Write each part
    start_idx = 0
    for i in range(num_parts):
        # Add one extra row to first 'remainder' parts
        part_size = rows_per_part + (1 if i < remainder else 0)
        end_idx = start_idx + part_size

        part_urls = urls_list[start_idx:end_idx]

        # Create output filename
        output_file = output_base.parent / f"{output_base.stem}_part{i+1}{output_base.suffix}"

        with open(output_file, 'w', encoding='utf-8') as f:
            for url in part_urls:
                f.write(url + '\n')

        output_size = output_file.stat().st_size / (1024 * 1024)
        print(f"  Part {i+1}: {len(part_urls):,} rows -> {output_file} ({output_size:.2f} MB)")

        start_idx = end_idx

    print("=" * 60)
    print(f"Successfully extracted URLs into {num_parts} parts")


def main():
    # CONFIGURE THESE PATHS
    input_file = Path("data/x_2024.csv")  # UPDATE THIS PATH
    output_base = Path("data/urls_extracted.txt")  # UPDATE THIS PATH

    # Check if input file exists
    if not input_file.exists():
        print(f"Error: Input file '{input_file}' does not exist.")
        print("\nPlease update the paths in this script:")
        print("  - input_file: Path to your CSV file")
        print("  - output_base: Base name for output files")
        sys.exit(1)

    # Extract URLs to text files
    try:
        extract_urls_to_txt(input_file, output_base, num_parts=4)
    except Exception as e:
        print(f"\nFailed to extract URLs: {e}")
        import traceback
        traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()
