import pandas as pd
from pathlib import Path

def merge_csv_files(input_directory, output_file):
    """
    Merge multiple CSV files vertically (one below the other).
    """
    try:
        # Get all CSV files in the directory
        csv_files = list(Path(input_directory).glob('*.csv'))

        if len(csv_files) == 0:
            print(f"No CSV files found in {input_directory}")
            return False

        print(f"Found {len(csv_files)} CSV files")

        # Initialize an empty list to store individual dataframes
        dfs = []

        # Read each CSV file with latin-1 encoding
        for file in csv_files:
            try:
                # Try reading with latin-1 encoding
                df = pd.read_csv(file, encoding='latin-1')
                dfs.append(df)
                print(f"Successfully read: {file.name}")
            except Exception as e:
                print(f"Error reading {file.name}: {str(e)}")
                continue

        if not dfs:
            print("No data frames to merge!")
            return False

        # Concatenate all dataframes vertically
        merged_df = pd.concat(dfs, axis=0, ignore_index=True)

        # Save the merged dataframe
        merged_df.to_csv(output_file, index=False, encoding='utf-8')
        print(f"\nMerged CSV file saved as: {output_file}")
        print(f"Total rows in merged file: {len(merged_df)}")

        return True

    except Exception as e:
        print(f"An error occurred: {str(e)}")
        return False