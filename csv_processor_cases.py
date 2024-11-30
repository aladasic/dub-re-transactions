import pandas as pd
from geopy.geocoders import Nominatim
from geopy.exc import GeocoderTimedOut, GeocoderServiceError
import time
import re
import json
import os
from tqdm import tqdm

# Cache for storing geocoding results
CACHE_FILE = 'geocoding_cache.json'


def load_cache():
    if os.path.exists(CACHE_FILE):
        with open(CACHE_FILE, 'r') as f:
            return json.load(f)
    return {}


def save_cache(cache):
    with open(CACHE_FILE, 'w') as f:
        json.dump(cache, f)


def clean_address_for_planning(address):
    """Enhanced address cleaning specifically for planning applications."""
    if not isinstance(address, str):
        return ""

    # Truncate very long addresses at certain markers
    markers = [" on lands at ", " The application site consists of ", " The Lands comprise of "]
    for marker in markers:
        if marker.lower() in address.lower():
            address = address.split(marker)[0]

    # Remove specific patterns that often cause issues
    patterns_to_remove = [
        r'\([^)]*\)',  # Remove anything in parentheses
        r'Protected Structure',
        r'Site to the rear of',
        r'Site to the north of',
        r'Public grass verge',
        r'Former',
        r'The application site',
        r'\b[A-Z]\d{2}\s*[A-Z0-9]{4}\b',  # Remove Eircode
        r'Co\.\s*Dublin',  # Remove Co. Dublin as we'll add Dublin later
        r'&\s*\.\.\.',  # Remove truncated parts
        r'Within the curtilage of',
        r'[^,]*\b(Service Station|Public House)\b',  # Remove business names
    ]

    for pattern in patterns_to_remove:
        address = re.sub(pattern, '', address, flags=re.IGNORECASE)

    # Handle special cases
    if 'junction' in address.lower():
        # For junctions, keep only the main road names
        roads = re.findall(r'([^,]+(?:Road|Street|Avenue|Lane))', address)
        if roads:
            address = ' and '.join(roads)

    # Clean up and standardize
    address = re.sub(r'\s+', ' ', address)  # Remove extra spaces
    address = re.sub(r',\s*,', ',', address)  # Remove empty elements
    address = re.sub(r'^\s*,\s*', '', address)  # Remove leading comma
    address = re.sub(r'\s*,\s*$', '', address)  # Remove trailing comma

    # Ensure it ends with Dublin, Ireland
    if not any(x in address.upper() for x in ['DUBLIN', 'D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7', 'D8', 'D9']):
        address += ', Dublin'
    if 'IRELAND' not in address.upper():
        address += ', Ireland'

    return address.strip()


def get_coordinates(address, geolocator):
    """Enhanced geocoding with better fallback options for planning applications."""
    original_address = address

    # Create multiple variants of the address to try
    address_variants = []

    # Clean the address
    cleaned_address = clean_address_for_planning(original_address)

    # Generate address variants
    address_variants = [
        cleaned_address,  # Try full cleaned address
        re.sub(r',.*Dublin', ', Dublin', cleaned_address),  # Just main location + Dublin
        re.sub(r'(.*?),.*?(Dublin.*)', r'\1, \2', cleaned_address),  # First part + Dublin
    ]

    # If we have a street name, try just the street + Dublin
    street_match = re.search(r'([^,]+(?:Road|Street|Avenue|Lane|Rise|Park|Place|Drive|Grove|Way))', cleaned_address)
    if street_match:
        address_variants.append(f"{street_match.group(1)}, Dublin, Ireland")

    # Try each variant
    for variant in address_variants:
        try:
            time.sleep(1)  # Respect usage limits
            location = geolocator.geocode(variant)

            if location:
                # Verify it's in greater Dublin area (expanded bounding box)
                if (52.8 <= location.latitude <= 53.8 and
                        -6.6 <= location.longitude <= -5.9):
                    return location.latitude, location.longitude

        except (GeocoderTimedOut, GeocoderServiceError) as e:
            continue
        except Exception as e:
            continue

    print(f"Could not find coordinates for address: {original_address}")
    return None, None


def process_planning_cases(input_file, output_file, max_rows=None):
    """Process the planning cases CSV file with enhanced geocoding."""
    # Read the CSV file
    df = pd.read_csv(input_file, dtype=str, low_memory=False)

    if max_rows:
        df = df.head(max_rows)
        print(f"Processing first {max_rows} rows...")

    # Initialize coordinate columns
    df['Latitude'] = None
    df['Longitude'] = None

    # Load cache
    cache = load_cache()

    # Process unique addresses
    unique_addresses = list(set(df.iloc[:, 1].dropna().unique()) - set(cache.keys()))

    if unique_addresses:
        print(f"\nProcessing {len(unique_addresses)} unique addresses...")
        geolocator = Nominatim(user_agent="dublin_planning_processor")

        # Track success rate
        success_count = 0
        total_count = len(unique_addresses)

        for address in tqdm(unique_addresses):
            if address and address not in cache:
                coords = get_coordinates(address, geolocator)
                cache[address] = coords
                if coords[0] is not None:
                    success_count += 1
                save_cache(cache)

        # Print success rate
        success_rate = (success_count / total_count) * 100
        print(f"\nGeocoding success rate: {success_rate:.1f}%")

    # Update DataFrame coordinates
    print("\nUpdating coordinates in DataFrame...")
    for idx, row in df.iterrows():
        address = row.iloc[1]  # Column B (index 1)
        if address in cache:
            df.at[idx, 'Latitude'], df.at[idx, 'Longitude'] = cache[address]

    # Move Latitude and Longitude columns to position K (index 10)
    cols = list(df.columns)
    cols.remove('Latitude')
    cols.remove('Longitude')
    cols.insert(10, 'Latitude')
    cols.insert(11, 'Longitude')
    df = df[cols]

    # Print statistics
    total_rows = len(df)
    rows_with_coords = df['Latitude'].notna().sum()
    print(f"\nFinal statistics:")
    print(f"Total rows: {total_rows}")
    print(f"Rows with coordinates: {rows_with_coords}")
    print(f"Overall success rate: {(rows_with_coords / total_rows) * 100:.1f}%")

    print("\nSample of processed data:")
    print(df.iloc[:, [1, 10, 11]].head())

    df.to_csv(output_file, index=False)
    print(f"\nProcessing complete. Results saved to {output_file}")


if __name__ == "__main__":
    process_planning_cases("planning_cases_merged.csv", "planning_cases_merged_processed.csv", max_rows=None)