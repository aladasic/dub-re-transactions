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


def clean_price(price_str):
    try:
        number = re.sub(r'[^0-9]', '', str(price_str))
        return float(number) / 100
    except:
        return None


def standardize_dublin_areas(address):
    """Standardize Dublin area names and postcodes."""
    area_mappings = {
        'DUBLIN 1': 'D1', 'DUBLIN 2': 'D2', 'DUBLIN 3': 'D3', 'DUBLIN 4': 'D4',
        'DUBLIN 5': 'D5', 'DUBLIN 6': 'D6', 'DUBLIN 7': 'D7', 'DUBLIN 8': 'D8',
        'DUBLIN 9': 'D9', 'DUBLIN 10': 'D10', 'DUBLIN 11': 'D11', 'DUBLIN 12': 'D12',
        'DUBLIN 13': 'D13', 'DUBLIN 14': 'D14', 'DUBLIN 15': 'D15', 'DUBLIN 16': 'D16',
        'DUBLIN 17': 'D17', 'DUBLIN 18': 'D18', 'DUBLIN 20': 'D20', 'DUBLIN 22': 'D22',
        'DUBLIN 24': 'D24',
        'BAC': 'Dublin',  # Irish language
        'BAILE ATHA CLIATH': 'Dublin',
        'CO DUBLIN': 'County Dublin',
        'CO. DUBLIN': 'County Dublin'
    }

    for old, new in area_mappings.items():
        address = re.sub(fr'\b{old}\b', new, address, flags=re.IGNORECASE)

    return address


def clean_address(address):
    """Enhanced address cleaning for better geocoding."""
    if not isinstance(address, str):
        return ""

    # Convert to uppercase for consistent processing
    address = address.upper()

    # Remove specific prefixes
    prefixes_to_remove = [
        r'^(APT|APARTMENT|UNIT|NO\.|FLAT)\s*\.?\s*\d+\s*,?\s*',
        r'^\d+[A-Z]?\s*,?\s*',
        r'APT\.?\s*\d+\s*-?\s*',
    ]

    for prefix in prefixes_to_remove:
        address = re.sub(prefix, '', address, flags=re.IGNORECASE)

    # Standardize common terms
    replacements = {
        'RD': 'ROAD',
        'ST': 'STREET',
        'AVE': 'AVENUE',
        'APTS': 'APARTMENTS',
        'DR': 'DRIVE',
        'LN': 'LANE',
        'CT': 'COURT',
        'CRES': 'CRESCENT',
        'SQ': 'SQUARE',
        'PK': 'PARK',
        'GDNS': 'GARDENS',
        'APT': 'APARTMENT'
    }

    for abbr, full in replacements.items():
        address = re.sub(fr'\b{abbr}\b', full, address)

    # Clean up any extra commas and spaces
    address = re.sub(r'\s+,\s+', ', ', address)
    address = re.sub(r'\s+', ' ', address)

    # Standardize Dublin areas
    address = standardize_dublin_areas(address)

    # Ensure it ends with Dublin, Ireland if not already present
    if 'DUBLIN' not in address and 'D\d{1,2}' not in address:
        address += ', DUBLIN'
    if 'IRELAND' not in address:
        address += ', IRELAND'

    return address.title()


def get_coordinates(address, geolocator):
    """Enhanced geocoding with better fallback options."""
    original_address = address
    tries = [
        lambda addr: addr,  # Try full address
        lambda addr: re.sub(r',.*Dublin', ', Dublin', addr),  # Simplify to main street + Dublin
        lambda addr: re.match(r'^([^,]+,[^,]+)', addr).group(1) + ', Dublin, Ireland'  # Just first two parts
    ]

    for try_num, address_modifier in enumerate(tries):
        try:
            cleaned_address = clean_address(original_address)
            search_address = address_modifier(cleaned_address)

            time.sleep(1)  # Respect usage limits
            location = geolocator.geocode(search_address)

            if location:
                # Verify it's in Dublin area (approximate bounding box)
                if (52.9 <= location.latitude <= 53.7 and
                        -6.5 <= location.longitude <= -6.0):
                    return location.latitude, location.longitude

        except (GeocoderTimedOut, GeocoderServiceError) as e:
            if try_num == len(tries) - 1:
                print(f"Failed to geocode {original_address}: {str(e)}")
            continue
        except Exception as e:
            continue

    print(f"Could not find coordinates for address: {original_address}")
    return None, None


def process_csv(input_file, output_file, max_rows=None):
    """Process the CSV file with enhanced geocoding."""
    # Read the CSV file
    df = pd.read_csv(input_file, dtype=str, low_memory=False)

    if max_rows:
        df = df.head(max_rows)
        print(f"Processing first {max_rows} rows...")

    # Convert date and extract month/year
    df['Date of Sale (dd/mm/yyyy)'] = pd.to_datetime(df['Date of Sale (dd/mm/yyyy)'], format='%d/%m/%Y')
    df['Sale_Month'] = df['Date of Sale (dd/mm/yyyy)'].dt.month
    df['Sale_Year'] = df['Date of Sale (dd/mm/yyyy)'].dt.year
    df['Date of Sale (dd/mm/yyyy)'] = df['Date of Sale (dd/mm/yyyy)'].dt.strftime('%d/%m/%Y')

    # Clean price
    df['Price_Cleaned'] = df.iloc[:, 4].apply(clean_price)

    # Initialize coordinate columns
    df['Latitude'] = None
    df['Longitude'] = None

    # Load cache
    cache = load_cache()

    # Process unique addresses
    unique_addresses = list(set(df['Address'].unique()) - set(cache.keys()))

    if unique_addresses:
        print(f"\nProcessing {len(unique_addresses)} unique addresses...")
        geolocator = Nominatim(user_agent="dublin_property_processor")

        for address in tqdm(unique_addresses):
            if address and address not in cache:
                coords = get_coordinates(address, geolocator)
                cache[address] = coords
                save_cache(cache)

    # Update DataFrame coordinates
    print("\nUpdating coordinates in DataFrame...")
    for idx, row in df.iterrows():
        address = row['Address']
        if address in cache:
            df.at[idx, 'Latitude'], df.at[idx, 'Longitude'] = cache[address]

    # Select and reorder columns
    columns_to_keep = ['Date of Sale (dd/mm/yyyy)', 'Sale_Month', 'Sale_Year',
                       'Address', 'County', 'Eircode',
                       'Price_Cleaned', 'Not Full Market Price', 'VAT Exclusive',
                       'Description of Property', 'Property Size Description',
                       'Latitude', 'Longitude']

    df = df[columns_to_keep]

    print("\nSample of processed data:")
    print(df[['Date of Sale (dd/mm/yyyy)', 'Sale_Month', 'Sale_Year',
              'Address', 'Price_Cleaned', 'Latitude', 'Longitude']].head())

    df.to_csv(output_file, index=False)
    print(f"\nProcessing complete. Results saved to {output_file}")


if __name__ == "__main__":
    process_csv("merged_dub_properties.csv", "merged_dub_properties_processed.csv", max_rows=None)