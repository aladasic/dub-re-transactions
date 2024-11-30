import os
from merger import merge_csv_files
from csv_processor import process_csv


def main():
    # Define paths
    input_dir = "./Data_DUB"
    merged_file = "./merged_dub_properties.csv"
    final_file = "./final_dublin_properties.csv"

    # Create input directory if it doesn't exist
    os.makedirs(input_dir, exist_ok=True)

    print("Step 1: Merging CSV files...")
    success = merge_csv_files(input_dir, merged_file)




if __name__ == "__main__":
    main()