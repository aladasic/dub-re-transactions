from bs4 import BeautifulSoup
import pandas as pd
import re
import requests
import time


def scrape_cases_from_url(url):
    # Fetch the webpage content
    print(f"Scraping URL: {url}")
    response = requests.get(url)
    if response.status_code != 200:
        print(f"Failed to fetch the webpage. Status code: {response.status_code}")
        return []

    return scrape_cases_from_html(response.text)


def scrape_cases_from_html(html_content):
    soup = BeautifulSoup(html_content, 'html.parser')
    cases = []

    # Find all case divs
    case_divs = soup.find_all('div', class_='cell')

    for case_div in case_divs:
        case = {}

        # Get the case link element
        case_link = case_div.find('a', class_='card-item')
        if not case_link:
            continue

        # Get case type (meta)
        meta = case_div.find('span', class_='meta')
        case['type'] = meta.text.strip() if meta else ''

        # Get title
        title = case_div.find('span', class_='title')
        case['title'] = title.text.strip() if title else ''

        # Get all details spans
        details_spans = case_div.find_all('span', class_='details')

        # Initialize default values
        case['reference'] = ''
        case['status'] = ''
        case['description'] = ''
        case['date_lodged'] = ''
        case['date_signed'] = ''
        case['eiar'] = ''
        case['nis'] = ''

        # Process each details span
        for span in details_spans:
            text = span.text.strip()

            if 'Case reference:' in text:
                case['reference'] = text.split('Case reference:')[1].strip()
            elif 'Status:' in text:
                case['status'] = text.split('Status:')[1].strip()
            elif 'Description:' in text:
                case['description'] = text.split('Description:')[1].strip()
            elif 'Date lodged:' in text:
                # Extract both lodged and signed dates
                date_parts = text.split(';')
                if len(date_parts) >= 1:
                    case['date_lodged'] = date_parts[0].split('Date lodged:')[1].strip()
                if len(date_parts) >= 2:
                    case['date_signed'] = date_parts[1].split('Signed:')[1].strip()
            elif 'EIAR:' in text:
                case['eiar'] = text.split('EIAR:')[1].strip()
            elif 'NIS:' in text:
                case['nis'] = text.split('NIS:')[1].strip()

        # Get parties
        parties_div = case_div.find('div', class_='details')
        if parties_div and parties_div.find('ul'):
            parties = []
            party_items = parties_div.find('ul').find_all('li')
            for party in party_items:
                party_text = ' '.join(party.stripped_strings)
                parties.append(party_text)
            case['parties'] = '; '.join(parties)
        else:
            case['parties'] = ''

        cases.append(case)

    return cases


def save_to_csv(cases, filename='planning_cases_merged.csv'):
    df = pd.DataFrame(cases)
    # Reorder columns for better readability
    column_order = ['type', 'title', 'reference', 'status', 'description',
                    'date_lodged', 'date_signed', 'eiar', 'nis', 'parties']
    df = df[column_order]
    df.to_csv(filename, index=False, encoding='utf-8-sig')
    print(f"Saved {len(df)} cases to {filename}")
    return df


def main():
    # List of URLs to scrape
    urls = [
        "https://www.pleanala.ie/en-ie/cases?lodgedto=2024-11-07&decisionfrom=2023-01-01&decisionto=2023-06-01&county=6",
        "https://www.pleanala.ie/en-ie/cases?lodgedto=2024-11-07&decisionfrom=2023-06-02&decisionto=2023-09-01&county=6",
        "https://www.pleanala.ie/en-ie/cases?lodgedto=2024-11-07&decisionfrom=2023-09-02&decisionto=2024-01-01&county=6",
        "https://www.pleanala.ie/en-ie/cases?lodgedto=2024-11-07&decisionfrom=2024-01-02&decisionto=2024-06-01&county=6",
        "https://www.pleanala.ie/en-ie/cases?lodgedto=2024-11-07&decisionfrom=2024-06-02&decisionto=2024-10-01&county=6",
        "https://www.pleanala.ie/en-ie/cases?lodgedto=2024-11-07&decisionfrom=2024-10-02&decisionto=2024-11-07&county=6"
    ]

    # List to store all cases
    all_cases = []

    # Scrape each URL
    for url in urls:
        cases = scrape_cases_from_url(url)
        all_cases.extend(cases)
        # Add a small delay between requests to be polite to the server
        time.sleep(2)

    # Save all cases to a single CSV
    if all_cases:
        print(f"\nTotal cases found: {len(all_cases)}")
        df = save_to_csv(all_cases)

        # Remove duplicates if any (based on case reference)
        df_unique = df.drop_duplicates(subset=['reference'])
        if len(df_unique) < len(df):
            print(f"Removed {len(df) - len(df_unique)} duplicate cases")
            df_unique.to_csv('planning_cases_merged.csv', index=False, encoding='utf-8-sig')
            print(f"Final number of unique cases: {len(df_unique)}")


if __name__ == "__main__":
    main()