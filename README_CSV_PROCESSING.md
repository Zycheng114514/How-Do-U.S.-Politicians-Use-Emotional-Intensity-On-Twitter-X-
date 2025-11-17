# CSV Processing Script

This script processes CSV files in `dataverse_files/xdata/` to keep only the first 8 columns.

## Features

- **Memory Efficient**: Processes files row-by-row, not loading entire files into memory
- **One-by-One Processing**: Handles each file sequentially to avoid RAM overflow
- **No Dependencies**: Uses only Python standard library (csv module)
- **Safe**: Creates temporary files and only replaces originals after successful processing
- **Progress Tracking**: Shows file size and row count progress

## Prerequisites

- Python 3.6 or higher (already installed)
- CSV files should be in: `dataverse_files/xdata/`

## Usage

1. **Ensure your CSV files are in the correct location**:
   ```bash
   # Create the directory if needed
   mkdir -p dataverse_files/xdata

   # Place your CSV files there
   # (Download or move them to dataverse_files/xdata/)
   ```

2. **Run the script**:
   ```bash
   python3 process_csv_files.py
   ```

   Or if you made it executable:
   ```bash
   ./process_csv_files.py
   ```

## What It Does

1. Scans `dataverse_files/xdata/` for all CSV files
2. For each CSV file:
   - Reads it row-by-row (memory efficient)
   - Keeps only the first 8 columns
   - Writes to a temporary file
   - Replaces the original file after successful processing
3. Shows progress for every 10,000 rows processed

## Example Output

```
Found 3 CSV file(s) to process
============================================================

[1/3] data1.csv
Processing: dataverse_files/xdata/data1.csv
  File size: 125.45 MB
  Processed 250000 rows... Done!
  ✓ Successfully updated dataverse_files/xdata/data1.csv

[2/3] data2.csv
Processing: dataverse_files/xdata/data2.csv
  File size: 98.32 MB
  Processed 180000 rows... Done!
  ✓ Successfully updated dataverse_files/xdata/data2.csv

============================================================
Processing complete!
```

## Safety

- Original files are only replaced after successful processing
- If an error occurs, the temporary file is cleaned up
- Original file remains untouched if processing fails

## Troubleshooting

**"Directory 'dataverse_files/xdata' does not exist"**
- Create the directory: `mkdir -p dataverse_files/xdata`
- Move your CSV files into it

**"No CSV files found"**
- Check that your files have `.csv` extension
- Verify they're in the correct directory

**Encoding errors**
- The script uses UTF-8 encoding
- If you have files with different encoding, edit the script to specify the correct encoding
