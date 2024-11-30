# Required Libraries
library(sf)          
library(dplyr)       
library(ggplot2)     
library(biscale)     
library(cowplot)     

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
    filter(Latitude >= 53.1 & Latitude <= 53.6) %>%  
    filter(Longitude >= -6.5 & Longitude <= -6.0)    
  
  # Print cleaning summary statistics
  cat("Property Data Cleaning Summary:\n")
  cat("----------------------------\n")
  cat("Original rows:", nrow(properties), "\n")
  cat("Rows after cleaning:", nrow(properties_clean), "\n")
  cat("Removed rows:", nrow(properties) - nrow(properties_clean), "\n\n")
  
  # Step 3: Convert cleaned properties to spatial (SF) object
  properties_sf <- st_as_sf(properties_clean, 
                            coords = c("Longitude", "Latitude"),
                            crs = 4326)
  
  # Step 4: Transform to ITM
  properties_itm <- st_transform(properties_sf, 2157)
  
  # Step 5: Add small buffer to handle precision issues
  dublin_buffered <- st_buffer(dublin_sf, dist = 1)
  
  # Step 6: Clean price data
  properties_itm$Price_Cleaned <- as.numeric(gsub("[^0-9.]", "", as.character(properties_itm$Price_Cleaned)))
  
  # Step 7: Perform spatial join
  properties_with_constituency <- st_join(properties_itm, 
                                          dublin_buffered,
                                          join = st_intersects,
                                          left = TRUE)
  
  # Print join statistics
  cat("Property Spatial Join Summary:\n")
  cat("--------------------------\n")
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

#' Process Planning Permission Data
#' 
#' @param planning_file Path to CSV file containing planning data
#' @param dublin_sf Dublin shapefile as SF object
#' @return Dataframe with planning statistics by constituency
process_planning_data <- function(planning_file, dublin_sf) {
  # Read planning data
  planning <- read.csv(planning_file)
  
  # Clean planning data
  planning_clean <- planning %>%
    filter(!is.na(Latitude) & !is.na(Longitude)) %>%
    filter(Latitude != 0 & Longitude != 0) %>%
    filter(Latitude >= 53.1 & Latitude <= 53.6) %>%
    filter(Longitude >= -6.5 & Longitude <= -6.0) %>%
    mutate(is_refused = grepl("refuse|withdrawn", status, ignore.case = TRUE))
  
  # Print status breakdown
  cat("\nApplication Status Breakdown:\n")
  cat("-------------------------\n")
  print(table(planning_clean$status))
  
  # Convert to SF object and transform
  planning_sf <- st_as_sf(planning_clean, 
                          coords = c("Longitude", "Latitude"),
                          crs = 4326)
  planning_itm <- st_transform(planning_sf, 2157)
  
  # Spatial join with constituencies
  dublin_buffered <- st_buffer(dublin_sf, dist = 1)
  planning_with_constituency <- st_join(planning_itm, 
                                        dublin_buffered,
                                        join = st_intersects,
                                        left = TRUE)
  
  # Calculate rejection statistics
  planning_stats <- planning_with_constituency %>%
    st_drop_geometry() %>%
    group_by(CONSTITUENCY) %>%
    summarise(
      total_applications = n(),
      rejected = sum(is_refused, na.rm = TRUE),
      rejection_rate = (rejected / total_applications) * 100,
      refused_count = sum(grepl("refuse", status, ignore.case = TRUE), na.rm = TRUE),
      withdrawn_count = sum(grepl("withdrawn", status, ignore.case = TRUE), na.rm = TRUE),
      invalid_count = sum(grepl("invalid", status, ignore.case = TRUE), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(!is.na(CONSTITUENCY))
  
  # Print summaries
  cat("\nPlanning Data Summary:\n")
  cat("-------------------\n")
  cat("Original rows:", nrow(planning), "\n")
  cat("Cleaned rows:", nrow(planning_clean), "\n")
  cat("Rows with constituency match:", nrow(planning_with_constituency), "\n\n")
  
  cat("Rejection Statistics by Constituency:\n")
  cat("----------------------------------\n")
  print(planning_stats %>% 
          arrange(desc(rejection_rate)) %>%
          select(CONSTITUENCY, 
                 total_applications, 
                 rejection_rate,
                 refused_count,
                 withdrawn_count,
                 invalid_count))
  
  return(planning_stats)
}

#' Calculate Price Statistics by Constituency
#' 
#' @param properties_with_constituency SF object containing matched properties
#' @return Dataframe with price statistics by constituency
calculate_price_stats <- function(properties_with_constituency) {
  properties_df <- st_drop_geometry(properties_with_constituency)
  
  price_stats <- properties_df %>%
    group_by(CONSTITUENCY) %>%
    summarise(
      med_price = median(Price_Cleaned, na.rm = TRUE),
      n_transactions = n(),
      .groups = 'drop'
    ) %>%
    filter(!is.na(CONSTITUENCY)) %>%
    arrange(desc(n_transactions))
  
  cat("\nPrice Statistics by Constituency:\n")
  cat("------------------------------\n")
  print(price_stats %>%
          select(CONSTITUENCY, n_transactions, med_price) %>%
          mutate(med_price = round(med_price, 2)) %>%
          arrange(desc(n_transactions)))
  
  return(price_stats)
}

#' Create Single Variable Choropleth Map
#' 
#' @param dublin_sf Dublin shapefile as SF object
#' @param price_stats Dataframe with price statistics
#' @return ggplot object
create_price_map <- function(dublin_sf, price_stats) {
  map_data <- dublin_sf %>%
    left_join(price_stats, by = "CONSTITUENCY")
  
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

#' Create Bivariate Map
#' 
#' @param dublin_sf Dublin shapefile as SF object
#' @param price_stats Dataframe with price statistics
#' @param planning_stats Dataframe with planning statistics
#' @return ggplot object
create_bivariate_map <- function(dublin_sf, price_stats, planning_stats) {
  map_data <- dublin_sf %>%
    left_join(price_stats, by = "CONSTITUENCY") %>%
    left_join(planning_stats, by = "CONSTITUENCY")
  
  map_data <- bi_class(map_data, 
                       x = med_price, 
                       y = rejection_rate, 
                       style = "quantile", 
                       dim = 3)
  
  map <- ggplot() +
    geom_sf(data = map_data, 
            aes(fill = bi_class), 
            color = "white", 
            size = 0.3) +
    bi_scale_fill(pal = "DkBlue2", dim = 3) +
    theme_void() +
    labs(title = "House Prices and Planning Permission Rejection Rates",
         subtitle = "By Dublin Constituency\nRejections include refused, withdrawn, and invalid applications")
  
  legend <- bi_legend(pal = "DkBlue2",
                      dim = 3,
                      xlab = "Higher Median Price →",
                      ylab = "Higher Rejection Rate →",
                      size = 6)
  
  ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0.7, 0.1, 0.3, 0.3)
}

#' Main Execution Function
#' 
#' @return List containing all analysis results
main <- function() {
  tryCatch({
    # Step 1: Read Dublin shapefile from current directory
    message("Reading Dublin shapefile...")
    map <- read_sf('Dublin_Electoral')
    if (nrow(map) == 0) stop("Empty shapefile")
    
    # Filter for Dublin county and prepare shapefile
    dublin1 <- dplyr::filter(map, COUNTY == "DUBLIN")
    dublin1 <- dublin1 %>% rename(CONSTITUENCY = ENGLISH)
    dublin2 <- st_transform(dublin1, 2157)
    
    # Validate CRS
    cat("Dublin shapefile CRS:", st_crs(dublin2)$input, "\n\n")
    
    # Step 2: Process property data
    message("Processing property data...")
    properties_matched <- process_property_data(
      'merged_dub_properties_processed.csv',
      dublin2
    )
    
    # Step 3: Process planning data
    message("Processing planning data...")
    planning_stats <- process_planning_data(
      'planning_cases_merged_processed.csv',
      dublin2
    )
    
    # Step 4: Calculate price statistics
    message("Calculating price statistics...")
    price_stats <- calculate_price_stats(properties_matched)
    
    # Step 5: Create maps
    message("Creating maps...")
    price_map <- create_price_map(dublin2, price_stats)
    bivariate_map <- create_bivariate_map(dublin2, price_stats, planning_stats)
    
    # Display maps
    print(price_map)
    print(bivariate_map)
    
    # Create final constituency statistics
    constituencies_stats <- price_stats %>%
      left_join(planning_stats, by = "CONSTITUENCY") %>%
      select(CONSTITUENCY, n_transactions, med_price, 
             total_applications, rejection_rate) %>%
      mutate(med_price = round(med_price, 2),
             rejection_rate = round(rejection_rate, 2)) %>%
      arrange(desc(n_transactions))
    
    cat("\nFinal Combined Statistics:\n")
    cat("------------------------\n")
    print(constituencies_stats)
    
    # Return all results
    return(list(
      price_stats = price_stats,
      planning_stats = planning_stats,
      dublin_sf = dublin2,
      properties = properties_matched,
      price_map = price_map,
      bivariate_map = bivariate_map,
      constituencies_stats = constituencies_stats
    ))
    
  }, error = function(e) {
    message("Error in main execution: ", e$message)
    print(e)
  })
}

# Run the analysis
results <- main()