# Required Libraries
library(sf)          
library(dplyr)       
library(ggplot2)     

#' Process and Clean Property Data
#' 
#' @param property_file Path to CSV file containing property data
#' @param dublin_sf Dublin shapefile as SF object
#' @return SF object with properties matched to constituencies
process_property_data <- function(property_file, dublin_sf) {
  # Step 1: Read the raw property data
  properties <- read.csv(property_file)
  
  # Step 2: Clean data by removing invalid coordinates and add validation
  properties_clean <- properties %>%
    filter(!is.na(Latitude) & !is.na(Longitude)) %>%
    filter(Latitude != 0 & Longitude != 0) %>%
    # Add coordinate validation for Dublin area
    filter(Latitude >= 53.1 & Latitude <= 53.6) %>%  
    filter(Longitude >= -6.5 & Longitude <= -6.0)    
  
  # Print cleaning summary statistics
  cat("Data Cleaning Summary:\n")
  cat("---------------------\n")
  cat("Original rows:", nrow(properties), "\n")
  cat("Rows after cleaning:", nrow(properties_clean), "\n")
  cat("Removed rows:", nrow(properties) - nrow(properties_clean), "\n\n")
  
  # Step 3: Convert cleaneda properties to spatial (SF) object
  properties_sf <- st_as_sf(properties_clean, 
                            coords = c("Longitude", "Latitude"),
                            crs = 4326)
  
  # Step 4: Transform both datasets to ITM
  properties_itm <- st_transform(properties_sf, 2157)
  
  # Step 5: Add small buffer to handle precision issues
  dublin_buffered <- st_buffer(dublin_sf, dist = 1)
  
  # Step 6: Clean price data
  properties_itm$Price_Cleaned <- as.numeric(gsub("[^0-9.]", "", as.character(properties_itm$Price_Cleaned)))
  
  # Step 7: Perform spatial join with error handling
  properties_with_constituency <- st_join(properties_itm, 
                                          dublin_buffered,
                                          join = st_intersects,
                                          left = TRUE)
  
  # Print join statistics
  cat("Spatial Join Summary:\n")
  cat("--------------------\n")
  cat("Total properties:", nrow(properties_itm), "\n")
  cat("Matched properties:", sum(!is.na(properties_with_constituency$CONSTITUENCY)), "\n")
  cat("Unmatched properties:", sum(is.na(properties_with_constituency$CONSTITUENCY)), "\n\n")
  
  # Save unmatched properties for inspection
  unmatched <- properties_with_constituency[is.na(properties_with_constituency$CONSTITUENCY), ]
  if(nrow(unmatched) > 0) {
    write.csv(st_drop_geometry(unmatched), "unmatched_properties.csv")
    cat("Unmatched properties saved to 'unmatched_properties.csv'\n")
  }
  
  return(properties_with_constituency)
}

#' Calculate Price Statistics by Constituency
#' 
#' @param properties_with_constituency SF object containing matched properties
#' @return Dataframe with summary statistics by constituency
calculate_price_stats <- function(properties_with_constituency) {
  # Convert SF object to regular dataframe
  properties_df <- st_drop_geometry(properties_with_constituency)
  
  # Calculate statistics by constituency
  price_stats <- properties_df %>%
    group_by(CONSTITUENCY) %>%
    summarise(
      med_price = median(Price_Cleaned, na.rm = TRUE),
      n_transactions = n(),
      .groups = 'drop'
    ) %>%
    filter(!is.na(CONSTITUENCY)) %>%
    arrange(desc(n_transactions))
  
  # Print the summary table
  cat("\nSummary Statistics by Constituency:\n")
  cat("----------------------------------\n")
  print(price_stats %>%
          select(CONSTITUENCY, n_transactions, med_price) %>%
          mutate(med_price = round(med_price, 2)) %>%
          arrange(desc(n_transactions)))
  
  return(price_stats)
}

#' Create Choropleth Map of Property Prices
#' 
#' @param dublin_sf Dublin shapefile as SF object
#' @param price_stats Dataframe with price statistics
#' @return ggplot object
create_price_map <- function(dublin_sf, price_stats) {
  # Join statistics back to spatial data
  map_data <- dublin_sf %>%
    left_join(price_stats, by = "CONSTITUENCY")
  
  # Create choropleth map
  ggplot(map_data) +
    geom_sf(aes(fill = med_price), color = "white", size = 0.3) +
    scale_fill_viridis_c(
      name = "Median Price (€)",
      labels = scales::comma,
      option = "B",
      na.value = "grey80"
    ) +
    theme_void() +
    labs(title = "Dublin Property Prices by Constituency",
         caption = paste("Number of constituencies:", nrow(price_stats)))
}

#' Main Execution Function
main <- function() {
  tryCatch({
    # Step 1: Read and prepare Dublin shapefile
    message("Reading Dublin shapefile...")
    map <- read_sf('./RData/Dublin_Electoral')
    if (nrow(map) == 0) stop("Empty shapefile")
    
    # Filter for Dublin county and rename columns
    dublin1 <- dplyr::filter(map, COUNTY == "DUBLIN")
    dublin1 <- dublin1 %>% rename(CONSTITUENCY = ENGLISH)
    
    # Transform to ITM
    dublin2 <- st_transform(dublin1, 2157)
    
    # Validate CRS
    cat("Dublin shapefile CRS:", st_crs(dublin2)$input, "\n\n")
    
    # Step 2: Process property data
    message("Processing property data...")
    properties_matched <- process_property_data(
      './RData/merged_dub_properties_processed.csv',
      dublin2
    )
    
    # Step 3: Calculate statistics
    message("Calculating statistics...")
    price_stats <- calculate_price_stats(properties_matched)
    
    # Step 4: Create and display map
    message("Creating map...")
    map <- create_price_map(dublin2, price_stats)
    print(map)
    
    # Return all results
    return(list(
      price_stats = price_stats,
      dublin2 = dublin2,
      properties = properties_matched,
      map = map
    ))
    
  }, error = function(e) {
    message("Error in main execution: ", e$message)
    print(e)
  })
}

# Run the analysis
results <- main()

# Extract variables
price_stats <- results$price_stats
dublin2 <- results$dublin2
properties <- results$properties

# Create constituencies_stats
if (!is.null(price_stats)) {
  constituencies_stats <- price_stats %>%
    select(CONSTITUENCY, n_transactions, med_price) %>%
    mutate(med_price = round(med_price, 2)) %>%
    arrange(desc(n_transactions))
  
  print("\nFinal Constituency Statistics:")
  print(constituencies_stats)
}