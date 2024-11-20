# Required Libraries
library(sf)          
library(dplyr)       
library(ggplot2)
library(biscale)     
library(cowplot)     

# Process planning permission data with updated rejection definition
process_planning_data <- function(planning_file, dublin_sf) {
  # Read planning data
  planning <- read.csv(planning_file)
  
  # Clean planning data
  planning_clean <- planning %>%
    # Remove rows with missing coordinates
    filter(!is.na(Latitude) & !is.na(Longitude)) %>%
    filter(Latitude != 0 & Longitude != 0) %>%
    # Filter for Dublin area coordinates
    filter(Latitude >= 53.1 & Latitude <= 53.6) %>%
    filter(Longitude >= -6.5 & Longitude <= -6.0) %>%
    # Create binary outcome for decisions including withdrawn and invalid
    mutate(is_refused = grepl("refuse|withdrawn", status, ignore.case = TRUE))
  
  # Print status breakdown before spatial join
  cat("\nApplication Status Breakdown:\n")
  cat("-------------------------\n")
  print(table(planning_clean$status))
  
  # Convert to SF object for spatial join
  planning_sf <- st_as_sf(planning_clean, 
                          coords = c("Longitude", "Latitude"),
                          crs = 4326)
  
  # Transform to ITM
  planning_itm <- st_transform(planning_sf, 2157)
  
  # Spatial join with constituencies
  dublin_buffered <- st_buffer(dublin_sf, dist = 1)
  planning_with_constituency <- st_join(planning_itm, 
                                        dublin_buffered,
                                        join = st_intersects,
                                        left = TRUE)
  
  # Calculate rejection rate by constituency
  planning_stats <- planning_with_constituency %>%
    st_drop_geometry() %>%
    group_by(CONSTITUENCY) %>%
    summarise(
      total_applications = n(),
      rejected = sum(is_refused, na.rm = TRUE),
      rejection_rate = (rejected / total_applications) * 100,
      # Break down types of rejections
      refused_count = sum(grepl("refuse", status, ignore.case = TRUE), na.rm = TRUE),
      withdrawn_count = sum(grepl("withdrawn", status, ignore.case = TRUE), na.rm = TRUE),
      invalid_count = sum(grepl("invalid", status, ignore.case = TRUE), na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(!is.na(CONSTITUENCY))
  
  # Print cleaning summary
  cat("\nPlanning Data Cleaning Summary:\n")
  cat("-----------------------------\n")
  cat("Original rows:", nrow(planning), "\n")
  cat("Cleaned rows:", nrow(planning_clean), "\n")
  cat("Rows with constituency match:", nrow(planning_with_constituency), "\n\n")
  
  # Print detailed rejection statistics
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

# Rest of the functions remain the same
calculate_price_stats <- function(properties_with_constituency) {
  properties_df <- st_drop_geometry(properties_with_constituency)
  
  price_stats <- properties_df %>%
    group_by(CONSTITUENCY) %>%
    summarise(
      med_price = median(Price_Cleaned, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    filter(!is.na(CONSTITUENCY))
  
  return(price_stats)
}

create_bivariate_map <- function(dublin_sf, price_stats, planning_stats) {
  # Join both statistics to spatial data
  map_data <- dublin_sf %>%
    left_join(price_stats, by = "CONSTITUENCY") %>%
    left_join(planning_stats, by = "CONSTITUENCY")
  
  # Create bivariate classes
  map_data <- bi_class(map_data, 
                       x = med_price, 
                       y = rejection_rate, 
                       style = "quantile", 
                       dim = 3)
  
  # Create map
  map <- ggplot() +
    geom_sf(data = map_data, 
            aes(fill = bi_class), 
            color = "white", 
            size = 0.3) +
    bi_scale_fill(pal = "DkBlue2", dim = 3) +
    theme_void() +
    labs(title = "House Prices and Planning Permission Rejection Rates",
         subtitle = "By Dublin Constituency\nRejections include refused, withdrawn, and invalid applications")
  
  # Create legend
  legend <- bi_legend(pal = "DkBlue2",
                      dim = 3,
                      xlab = "Higher Median Price →",
                      ylab = "Higher Rejection Rate →",
                      size = 6)
  
  # Combine map and legend
  finalPlot <- ggdraw() +
    draw_plot(map, 0, 0, 1, 1) +
    draw_plot(legend, 0.7, 0.1, 0.3, 0.3)
  
  return(finalPlot)
}

main <- function() {
  tryCatch({
    # Read Dublin shapefile
    message("Reading Dublin shapefile...")
    map <- read_sf('./RData/Dublin_Electoral')
    dublin1 <- dplyr::filter(map, COUNTY == "DUBLIN") %>% 
      rename(CONSTITUENCY = ENGLISH)
    dublin2 <- st_transform(dublin1, 2157)
    
    # Process property data
    message("Processing property data...")
    properties_matched <- process_property_data(
      './RData/merged_dub_properties_processed.csv',
      dublin2
    )
    
    # Process planning data
    message("Processing planning data...")
    planning_stats <- process_planning_data(
      './RData/planning_cases_merged_processed.csv',
      dublin2
    )
    
    # Calculate price statistics
    message("Calculating price statistics...")
    price_stats <- calculate_price_stats(properties_matched)
    
    # Create and display bivariate map
    message("Creating bivariate map...")
    bivariate_map <- create_bivariate_map(dublin2, price_stats, planning_stats)
    print(bivariate_map)
    
    # Return all results
    return(list(
      price_stats = price_stats,
      planning_stats = planning_stats,
      dublin2 = dublin2,
      map = bivariate_map
    ))
    
  }, error = function(e) {
    message("Error in main execution: ", e$message)
    print(e)
  })
}

# Run the analysis
results <- main()