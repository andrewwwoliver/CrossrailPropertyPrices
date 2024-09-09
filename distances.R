# Load necessary libraries
library(r5r)
library(sf)  # For handling sf objects
library(dplyr)  # For data manipulation

# Allocate RAM to Java 
options(java.parameters = "-Xmx16G")  # Increase memory allocation as needed

# Define R5R directory
r5r_dir <- 'C:/Users/aoliv/OneDrive - University of Glasgow/Diss - Second draft/rail_router'

# Set up the network, assuming r5r supports timezone setting
r5r_core <- setup_r5(data_path = r5r_dir, verbose = FALSE)

# Load the data
load("test_properties_with_census.RData")
load("stops_unique.RData")  # Assuming stops_unique contains the station locations

# Remove id.y and rename id.x to id
properties_with_census <- properties_with_census %>%
  select(-id.y) %>%
  rename(id = id.x)

# Split the data into different dataframes based on the nearest station
split_data <- split(properties_with_census, properties_with_census$nearest_station)

# Sanitize names to make them valid R variable names
sanitize_name <- function(name) {
  name <- gsub("[^a-zA-Z0-9_]", "_", name) # Replace non-alphanumeric characters with _
  name <- gsub("^([0-9])", "_\\1", name)   # Add underscore if name starts with a digit
  return(name)
}

calculate_distances <- function(subset_data, station_location) {
  # Ensure subset_data is an sf object
  if (!inherits(subset_data, "sf")) {
    subset_data <- st_as_sf(subset_data)
  }
  
  # Extract latitude and longitude from the geometry column
  coords <- sf::st_coordinates(subset_data)
  origins <- data.frame(id = subset_data$id, lat = coords[, 2], lon = coords[, 1])
  
  destinations <- data.frame(id = station_location$id, lat = station_location$latitude, lon = station_location$longitude)
  
  # Calculate distances using detailed_itineraries without output directory
  detailed_routes <- detailed_itineraries(r5r_core, origins, destinations, mode = "WALK")
  
  # Check if detailed_routes has any rows
  if (nrow(detailed_routes) == 0) {
    warning(paste("No routes found for station", station_location$name))
    subset_data$distance_to_station <- NA
    return(subset_data)
  }
  
  # Extract the distance from detailed_routes
  distances <- aggregate(distance ~ from_id, data = detailed_routes, FUN = sum)
  
  # Check if distances has any rows
  if (nrow(distances) == 0) {
    warning(paste("No distances to aggregate for station", station_location$name))
    subset_data$distance_to_station <- NA
    return(subset_data)
  }
  
  # Merge the distances back to the original dataframe
  subset_data$distance_to_station <- distances$distance[match(subset_data$id, distances$from_id)]
  
  return(subset_data)
}

# Process each subset
for (station in names(split_data)) {
  sanitized_station <- sanitize_name(station)
  subset_data <- split_data[[station]]
  
  # Find the station location
  station_location <- stops_unique[stops_unique$name == station, ]
  
  # Debugging output
  cat("Processing station:", station, "\n")
  cat("Number of properties:", nrow(subset_data), "\n")
  print("Station location:")
  print(station_location)
  
  # Calculate distances
  processed_data <- calculate_distances(subset_data, station_location)
  
  # Save the subset as an RData file
  save(processed_data, file = paste0(sanitized_station, ".RData"))
  
  # Print progress for every 1,000 properties processed
  total_properties <- nrow(processed_data)
  if (total_properties > 0) {
    steps <- seq(1000, total_properties, by = 1000)
    if (length(steps) == 0) steps <- total_properties  # Ensure at least one step
    for (i in steps) {
      cat("Processed", min(i, total_properties), "properties for station", station, "\n")
    }
  }
}


#visualise road network and stations
# Load necessary libraries
library(r5r)
library(sf)
library(tidyverse)
library(ggplot2)

# Extract the road network as an sf object
road_network_sf <- street_network_to_sf(r5r_core)


# Extract the edges and vertices from the road network
edges_sf <- road_network_sf$edges
vertices_sf <- road_network_sf$vertices

# Ensure both are sf objects (optional check)
if (!inherits(edges_sf, "sf")) {
  stop("edges_sf is not an sf object")
}

if (!inherits(vertices_sf, "sf")) {
  stop("vertices_sf is not an sf object")
}

# Load necessary libraries
library(sf)
library(ggplot2)
library(dplyr)


# Write edges and vertices to shapefiles
st_write(edges_sf, "road_network_edges.shp")
st_write(vertices_sf, "road_network_vertices.shp")

# Load edges and vertices from shapefiles
edges_sf <- st_read("road_network_edges.shp")
vertices_sf <- st_read("road_network_vertices.shp")
# Define the bounding box coordinates
bbox <- c(xmin = -1.02, xmax = 0.4, ymin = 51.4, ymax = 51.7)


# Convert stops_unique to an sf object, assuming it has 'latitude' and 'longitude' columns
stops_sf <- st_as_sf(stops_unique, coords = c("longitude", "latitude"), crs = st_crs(edges_sf))

# Check if edges_sf and stops_sf are in the same CRS
if (st_crs(edges_sf) != st_crs(stops_sf)) {
  stop("CRS mismatch between edges_sf and stops_sf")
}

# Filter edges and vertices to the bounding box
edges_sf_filtered <- edges_sf %>% st_crop(bbox)
vertices_sf_filtered <- vertices_sf %>% st_crop(bbox)

# Filter stops to the bounding box
stops_sf_filtered <- stops_sf %>% st_intersection(st_as_sf(st_bbox(bbox)))

# Plot the road network (edges) and stops on a map
ggplot() +
  geom_sf(data = edges_sf_filtered, color = "grey", size = 0.5) +  # Plot the road network (edges)
  geom_sf(data = stops_sf, color = "red", size = 2, shape = 21, fill = "yellow") +  # Plot the stops
  theme_minimal() +  # Use a minimal theme
  labs(title = "Filtered Road Network with Stops", x = "Longitude", y = "Latitude")




###################
# Load necessary libraries
library(r5r)
library(sf)  # For handling sf objects
library(dplyr)  # For data manipulation

# Allocate RAM to Java 
options(java.parameters = "-Xmx16G")  # Increase memory allocation as needed

# Define R5R directory
r5r_dir <- 'C:/Users/aoliv/OneDrive - University of Glasgow/Diss - Second draft/rail_router'

# Set up the network, assuming r5r supports timezone setting
r5r_core <- setup_r5(data_path = r5r_dir, verbose = FALSE)

# Load the data
load("test_properties_with_census.RData")
load("stops_unique.RData")  # Assuming stops_unique contains the station locations

# Remove id.y and rename id.x to id
properties_with_census <- properties_with_census %>%
  select(-id.y) %>%
  rename(id = id.x)

# Replace the coordinates of Heathrow stations and Reading Rail Station with the closest vertices
stops_unique <- stops_unique %>%
  mutate(
    latitude = ifelse(name == "Heathrow Terminals 2 & 3 Rail Station", 51.481221,
                      ifelse(name == "Heathrow Terminal 4", 51.457904,
                             ifelse(name == "Reading Rail Station", 51.457940, latitude))),
    longitude = ifelse(name == "Heathrow Terminals 2 & 3 Rail Station", -0.453066,
                       ifelse(name == "Heathrow Terminal 4", -0.443737,
                              ifelse(name == "Reading Rail Station", -0.970600, longitude)))
  )

# Split the data into different dataframes based on the nearest station
split_data <- split(properties_with_census, properties_with_census$nearest_station)

# Sanitize names to make them valid R variable names
sanitize_name <- function(name) {
  name <- gsub("[^a-zA-Z0-9_]", "_", name) # Replace non-alphanumeric characters with _
  name <- gsub("^([0-9])", "_\\1", name)   # Add underscore if name starts with a digit
  return(name)
}

calculate_distances <- function(subset_data, station_location) {
  # Ensure subset_data is an sf object
  if (!inherits(subset_data, "sf")) {
    subset_data <- st_as_sf(subset_data)
  }
  
  # Extract latitude and longitude from the geometry column
  coords <- sf::st_coordinates(subset_data)
  origins <- data.frame(id = subset_data$id, lat = coords[, 2], lon = coords[, 1])
  
  destinations <- data.frame(id = station_location$id, lat = station_location$latitude, lon = station_location$longitude)
  
  # Calculate distances using detailed_itineraries without output directory
  detailed_routes <- detailed_itineraries(r5r_core, origins, destinations, mode = "WALK")
  
  # Check if detailed_routes has any rows
  if (nrow(detailed_routes) == 0) {
    warning(paste("No routes found for station", station_location$name))
    subset_data$distance_to_station <- NA
    return(subset_data)
  }
  
  # Extract the distance from detailed_routes
  distances <- aggregate(distance ~ from_id, data = detailed_routes, FUN = sum)
  
  # Check if distances has any rows
  if (nrow(distances) == 0) {
    warning(paste("No distances to aggregate for station", station_location$name))
    subset_data$distance_to_station <- NA
    return(subset_data)
  }
  
  # Merge the distances back to the original dataframe
  subset_data$distance_to_station <- distances$distance[match(subset_data$id, distances$from_id)]
  
  return(subset_data)
}

# Initialize a list to store processed dataframes
processed_data_list <- list()

# Process each subset
for (station in names(split_data)) {
  sanitized_station <- sanitize_name(station)
  subset_data <- split_data[[station]]
  
  # Find the station location
  station_location <- stops_unique[stops_unique$name == station, ]
  
  # Debugging output
  cat("Processing station:", station, "\n")
  cat("Number of properties:", nrow(subset_data), "\n")
  print("Station location:")
  print(station_location)
  
  # Calculate distances
  processed_data <- calculate_distances(subset_data, station_location)
  
  # Add the processed dataframe to the list
  processed_data_list[[station]] <- processed_data
}

# Combine all processed dataframes into one
final_combined_data <- bind_rows(processed_data_list)

# Save the combined data as an RData file
save(final_combined_data, file = "Distances/Combined_Processed_Data.RData")

# load final combined
load("Distances/Combined_Processed_Data.RData")


################---------------############
###################
# Load necessary libraries
library(r5r)
library(sf)  # For handling sf objects
library(dplyr)  # For data manipulation

# Allocate RAM to Java 
options(java.parameters = "-Xmx16G")  # Increase memory allocation as needed

# Define R5R directory
r5r_dir <- 'C:/Users/aoliv/OneDrive - University of Glasgow/Diss - Second draft/rail_router'

# Set up the network, assuming r5r supports timezone setting
r5r_core <- setup_r5(data_path = r5r_dir, verbose = FALSE)

# Load the data
load("test_properties_with_census.RData")
load("stops_unique.RData")  # Assuming stops_unique contains the station locations

# Remove id.y and rename id.x to id
properties_with_census <- properties_with_census %>%
  select(-id.y) %>%
  rename(id = id.x)

# Replace the coordinates of Heathrow stations and Reading Rail Station with the closest vertices
stops_unique <- stops_unique %>%
  mutate(
    latitude = ifelse(name == "Heathrow Terminals 2 & 3 Rail Station", 51.481221,
                      ifelse(name == "Heathrow Terminal 4", 51.457904,
                             ifelse(name == "Reading Rail Station", 51.457940, latitude))),
    longitude = ifelse(name == "Heathrow Terminals 2 & 3 Rail Station", -0.453066,
                       ifelse(name == "Heathrow Terminal 4", -0.443737,
                              ifelse(name == "Reading Rail Station", -0.970600, longitude)))
  )


# Extract latitude and longitude from the geometry column
extract_lat_lon <- function(geometry) {
  coords <- st_coordinates(geometry)
  return(list(latitude = coords[2], longitude = coords[1]))
}

# Function to calculate the second and third nearest distances and stations
calculate_second_third_nearest <- function(latitude, longitude, stops, nearest_station) {
  # Calculate distances from the property to all stops
  distances <- distGeo(matrix(c(longitude, latitude), nrow = 1), stops[, c("longitude", "latitude")])
  
  # Remove the distance to the nearest station already known
  stops_filtered <- stops[stops$name != nearest_station, ]
  distances_filtered <- distances[stops$name != nearest_station]
  
  # Sort the filtered distances and get the indices of the two smallest
  sorted_indices <- order(distances_filtered)
  
  # Get the second nearest and third nearest
  second_nearest <- sorted_indices[1]
  third_nearest <- sorted_indices[2]
  
  list(
    second_nearest_distance = distances_filtered[second_nearest], 
    second_nearest_station = stops_filtered$name[second_nearest],
    third_nearest_distance = distances_filtered[third_nearest], 
    third_nearest_station = stops_filtered$name[third_nearest]
  )
}

# Apply the function to each row in properties_with_census
nearest_info <- apply(properties_with_census, 1, function(row) {
  coords <- extract_lat_lon(row["geometry"][[1]])
  result <- calculate_second_third_nearest(
    coords$latitude, 
    coords$longitude, 
    stops_unique, 
    as.character(row["nearest_station"])
  )
  c(result$second_nearest_distance, result$second_nearest_station, 
    result$third_nearest_distance, result$third_nearest_station)
})

# Convert the result to a data frame and add it to properties_with_census
nearest_info_df <- data.frame(matrix(unlist(nearest_info), ncol = 4, byrow = TRUE), stringsAsFactors = FALSE)
names(nearest_info_df) <- c("second_nearest_distance", "second_nearest_station", 
                            "third_nearest_distance", "third_nearest_station")

# Bind the new information to the original properties_with_census
properties_with_census <- cbind(properties_with_census, nearest_info_df)

# Convert distances to numeric
properties_with_census$second_nearest_distance <- as.numeric(properties_with_census$second_nearest_distance)
properties_with_census$third_nearest_distance <- as.numeric(properties_with_census$third_nearest_distance)



## second

# Split the data into different dataframes based on the nearest station
split_data <- split(properties_with_census, properties_with_census$second_nearest_station)

# Sanitize names to make them valid R variable names
sanitize_name <- function(name) {
  name <- gsub("[^a-zA-Z0-9_]", "_", name) # Replace non-alphanumeric characters with _
  name <- gsub("^([0-9])", "_\\1", name)   # Add underscore if name starts with a digit
  return(name)
}

calculate_distances <- function(subset_data, station_location) {
  # Ensure subset_data is an sf object
  if (!inherits(subset_data, "sf")) {
    subset_data <- st_as_sf(subset_data)
  }
  
  # Extract latitude and longitude from the geometry column
  coords <- sf::st_coordinates(subset_data)
  origins <- data.frame(id = subset_data$id, lat = coords[, 2], lon = coords[, 1])
  
  destinations <- data.frame(id = station_location$id, lat = station_location$latitude, lon = station_location$longitude)
  
  # Calculate distances using detailed_itineraries without output directory
  detailed_routes <- detailed_itineraries(r5r_core, origins, destinations, mode = "WALK")
  
  # Check if detailed_routes has any rows
  if (nrow(detailed_routes) == 0) {
    warning(paste("No routes found for station", station_location$name))
    subset_data$distance_to_station <- NA
    return(subset_data)
  }
  
  # Extract the distance from detailed_routes
  distances <- aggregate(distance ~ from_id, data = detailed_routes, FUN = sum)
  
  # Check if distances has any rows
  if (nrow(distances) == 0) {
    warning(paste("No distances to aggregate for station", station_location$name))
    subset_data$distance_to_station <- NA
    return(subset_data)
  }
  
  # Merge the distances back to the original dataframe
  subset_data$distance_to_station <- distances$distance[match(subset_data$id, distances$from_id)]
  
  return(subset_data)
}

# Initialize a list to store processed dataframes
processed_data_list <- list()

# Process each subset
for (station in names(split_data)) {
  sanitized_station <- sanitize_name(station)
  subset_data <- split_data[[station]]
  
  # Find the station location
  station_location <- stops_unique[stops_unique$name == station, ]
  
  # Debugging output
  cat("Processing station:", station, "\n")
  cat("Number of properties:", nrow(subset_data), "\n")
  print("Station location:")
  print(station_location)
  
  # Calculate distances
  processed_data <- calculate_distances(subset_data, station_location)
  
  # Add the processed dataframe to the list
  processed_data_list[[station]] <- processed_data
}

# Combine all processed dataframes into one
final_combined_data <- bind_rows(processed_data_list)

# Save the combined data as an RData file
save(final_combined_data, file = "Distances/Second_Combined_Processed_Data.RData")





## third#################################


# Split the data into different dataframes based on the nearest station
split_data <- split(properties_with_census, properties_with_census$third_nearest_station)

# Sanitize names to make them valid R variable names
sanitize_name <- function(name) {
  name <- gsub("[^a-zA-Z0-9_]", "_", name) # Replace non-alphanumeric characters with _
  name <- gsub("^([0-9])", "_\\1", name)   # Add underscore if name starts with a digit
  return(name)
}

calculate_distances <- function(subset_data, station_location) {
  # Ensure subset_data is an sf object
  if (!inherits(subset_data, "sf")) {
    subset_data <- st_as_sf(subset_data)
  }
  
  # Extract latitude and longitude from the geometry column
  coords <- sf::st_coordinates(subset_data)
  origins <- data.frame(id = subset_data$id, lat = coords[, 2], lon = coords[, 1])
  
  destinations <- data.frame(id = station_location$id, lat = station_location$latitude, lon = station_location$longitude)
  
  # Calculate distances using detailed_itineraries without output directory
  detailed_routes <- detailed_itineraries(r5r_core, origins, destinations, mode = "WALK")
  
  # Check if detailed_routes has any rows
  if (nrow(detailed_routes) == 0) {
    warning(paste("No routes found for station", station_location$name))
    subset_data$distance_to_station <- NA
    return(subset_data)
  }
  
  # Extract the distance from detailed_routes
  distances <- aggregate(distance ~ from_id, data = detailed_routes, FUN = sum)
  
  # Check if distances has any rows
  if (nrow(distances) == 0) {
    warning(paste("No distances to aggregate for station", station_location$name))
    subset_data$distance_to_station <- NA
    return(subset_data)
  }
  
  # Merge the distances back to the original dataframe
  subset_data$distance_to_station <- distances$distance[match(subset_data$id, distances$from_id)]
  
  return(subset_data)
}

# Initialize a list to store processed dataframes
processed_data_list <- list()

# Process each subset
for (station in names(split_data)) {
  sanitized_station <- sanitize_name(station)
  subset_data <- split_data[[station]]
  
  # Find the station location
  station_location <- stops_unique[stops_unique$name == station, ]
  
  # Debugging output
  cat("Processing station:", station, "\n")
  cat("Number of properties:", nrow(subset_data), "\n")
  print("Station location:")
  print(station_location)
  
  # Calculate distances
  processed_data <- calculate_distances(subset_data, station_location)
  
  # Add the processed dataframe to the list
  processed_data_list[[station]] <- processed_data
}

# Combine all processed dataframes into one
final_combined_data <- bind_rows(processed_data_list)

# Save the combined data as an RData file
save(final_combined_data, file = "Distances/Third_Combined_Processed_Data.RData")



# QA Results
#Load second
# Load the RData file
load("Distances/Second_Combined_Processed_Data.RData")
second_final_combined_data <- final_combined_data
rm(final_combined_data)
#rename distance_to_station to distance_second_station
second_final_combined_data <- second_final_combined_data %>%
  rename(distance_second_station = distance_to_station)


load("Distances/Third_Combined_Processed_Data.RData")
third_final_combined_data <- final_combined_data
rm(final_combined_data)

#rename distance_to_station to distance_third_station
third_final_combined_data <- third_final_combined_data %>%
  rename(distance_third_station = distance_to_station)


# Load the RData file
load("Distances/Combined_Processed_Data.RData")

#rename distance_to_station to distance_first_station
final_combined_data <- final_combined_data %>%
  rename(distance_first_station = distance_to_station)

# Select the relevant columns from each dataframe
final_subset <- final_combined_data[, c('id', 'nearest_station', 'nearest_crossrail', 'distance_first_station')]
second_subset <- second_final_combined_data[, c('second_nearest_distance', 'second_nearest_station', 'distance_second_station')]
third_subset <- third_final_combined_data[, c('third_nearest_distance', 'third_nearest_station', 'distance_third_station')]

# Combine the selected columns into a new dataframe
distances_dataset <- data.frame(final_subset, second_subset, third_subset)

# Inspect the new dataset
head(distances_dataset)
# make nearest crossrail, and all the distance columns numeric
distances_dataset$nearest_crossrail <- as.numeric(distances_dataset$nearest_crossrail)
distances_dataset$distance_first_station <- as.numeric(distances_dataset$distance_first_station)
distances_dataset$second_nearest_distance <- as.numeric(distances_dataset$second_nearest_distance)
distances_dataset$distance_second_station <- as.numeric(distances_dataset$distance_second_station)
distances_dataset$third_nearest_distance <- as.numeric(distances_dataset$third_nearest_distance)
distances_dataset$distance_third_station <- as.numeric(distances_dataset$distance_third_station)

# how many rows is distance_second_station smaller than distance_first_station
# Count the number of rows where distance_second_station is smaller than distance_first_station, ignoring NA values
count_smaller <- sum(distances_dataset$distance_third_station < distances_dataset$distance_first_station, na.rm = TRUE)

# Print the result
count_smaller

# Create a new dataframe with the minimum distance and corresponding station
min_distance_dataset <- distances_dataset %>%
  mutate(
    min_distance = pmin(distance_first_station, distance_second_station, distance_third_station, na.rm = TRUE),
    nearest_crossrail_network = case_when(
      min_distance == distance_first_station ~ nearest_station,
      min_distance == distance_second_station ~ second_nearest_station,
      min_distance == distance_third_station ~ third_nearest_station,
      TRUE ~ NA_character_  # Handle any other cases, though there should be none
    )
  ) %>%
  select(min_distance, nearest_crossrail_network, id)

# Inspect the new dataframe
head(min_distance_dataset)

#rename min_distance to road_distance_crossrail
min_distance_dataset <- min_distance_dataset %>%
  rename(road_distance_crossrail = min_distance)

# save min_distance_dataset as rdata
save(min_distance_dataset, file = "Distances/Min_Distance_Dataset.RData")


#### EMPLOYMENT CENTRES --------------------------------------------
library(sf)
library(dplyr)

# load properties_with_census_df
load("lsoa.RData")

# centroids for each lsoa
lsoa_weights <- read.csv("LSOA_Dec_2011_WEIGHTS.csv")

# Convert to sf object with the British National Grid CRS
lsoa_weights_sf <- st_as_sf(lsoa_weights, coords = c("x", "y"), crs = 27700)

# Transform the CRS to WGS 84 (EPSG:4326)
lsoa_weights_sf <- st_transform(lsoa_weights_sf, crs = 4326)

#define employment above 20000
lsoa$employment_high <- ifelse(lsoa$Employment > 10000, 1, 0)

high_employment_lsoa <- lsoa %>%
  filter(employment_high == 1) %>%
  select(LSOA11CD)

lsoa_weights_high_employment <- lsoa_weights_sf %>%
  filter(LSOA11CD %in% high_employment_lsoa$LSOA11CD)
#load properties_with_census_df
load("properties_with_census_df.RData")

#routing
# Load necessary libraries
library(r5r)
library(sf)
library(dplyr)

# Allocate RAM to Java
options(java.parameters = "-Xmx16G")

# Define R5R directory
r5r_dir <- 'C:/Users/aoliv/OneDrive - University of Glasgow/Diss - Second draft/road_router'

# Set up the R5R core
r5r_core <- setup_r5(data_path = r5r_dir, verbose = FALSE)

#make properties_with_census sf
properties_sf <- st_as_sf(properties_with_census_df, crs = 4326)
#filter properties_sf to only include postcode and gemotry
properties_sf <- properties_sf %>%
  select(id.x, postcode, geometry)

#rename id.x to id 
properties_sf <- properties_sf %>%
  rename(id = id.x)

#remove duplicates in postcode column
properties_sf <- properties_sf %>%
  distinct(postcode, geometry, .keep_all = TRUE)


  

library(sf)
library(dplyr)
library(geosphere) # For calculating Euclidean distances
library(r5r)


#how many unique postcodes in properties_sf
unique_postcodes <- n_distinct(properties_sf$postcode)

head(lsoa_weights_high_employment)

###############
library(sf)
library(geosphere)

# Function to calculate the nearest, second nearest, and third nearest LSOA11CD
calculate_nearest_lsoa <- function(property_point, lsoa_data) {
  # Extract coordinates from the property point
  property_coords <- st_coordinates(property_point)
  
  # Extract coordinates from LSOA11CD points
  lsoa_coords <- st_coordinates(lsoa_data)
  
  # Calculate distances from the property to all LSOA11CD centroids
  distances <- distGeo(property_coords, lsoa_coords)
  
  # Get indices of the three smallest distances
  sorted_indices <- order(distances)[1:3]
  
  # Return the nearest, second nearest, and third nearest distances and corresponding LSOA11CDs
  list(
    nearest_distance = distances[sorted_indices[1]],
    nearest_LSOA11CD = lsoa_data$LSOA11CD[sorted_indices[1]],
    second_nearest_distance = distances[sorted_indices[2]],
    second_nearest_LSOA11CD = lsoa_data$LSOA11CD[sorted_indices[2]],
    third_nearest_distance = distances[sorted_indices[3]],
    third_nearest_LSOA11CD = lsoa_data$LSOA11CD[sorted_indices[3]]
  )
}

# Apply the function to each geometry in properties_sf
nearest_info <- lapply(st_geometry(properties_sf), function(property_point) {
  result <- calculate_nearest_lsoa(property_point, lsoa_weights_high_employment)
  c(
    result$nearest_distance, result$nearest_LSOA11CD,
    result$second_nearest_distance, result$second_nearest_LSOA11CD,
    result$third_nearest_distance, result$third_nearest_LSOA11CD
  )
})

# Convert the result to a data frame and add it to properties_sf
nearest_info_df <- do.call(rbind, lapply(nearest_info, function(x) {
  data.frame(
    nearest_distance = x[1], nearest_LSOA11CD = x[2],
    second_nearest_distance = x[3], second_nearest_LSOA11CD = x[4],
    third_nearest_distance = x[5], third_nearest_LSOA11CD = x[6],
    stringsAsFactors = FALSE
  )
}))

# Bind the new columns to the properties_sf data frame
properties_sf <- cbind(properties_sf, nearest_info_df)

# Convert distance columns to numeric
properties_sf$nearest_distance <- as.numeric(properties_sf$nearest_distance)
properties_sf$second_nearest_distance <- as.numeric(properties_sf$second_nearest_distance)
properties_sf$third_nearest_distance <- as.numeric(properties_sf$third_nearest_distance)

# Frequency of each nearest, second nearest, and third nearest LSOA11CD
table(properties_sf$nearest_LSOA11CD)
table(properties_sf$second_nearest_LSOA11CD)
table(properties_sf$third_nearest_LSOA11CD)

#how many nas in nearest_distance
sum(is.na(properties_sf$nearest_distance))

# Load required packages
library(sf)
library(r5r)
library(dplyr)

# Set up the r5r environment
options(java.parameters = "-Xmx16G")
r5r_dir <- 'C:/Users/aoliv/OneDrive - University of Glasgow/Diss - Second draft/rail_router'
r5r_core <- setup_r5(data_path = r5r_dir, verbose = FALSE)

# Prepare origins (properties) and destinations (nearest LSOA11CD)
origins <- properties_sf %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(lat_origin = Y, lon_origin = X) %>%
  mutate(postcode_id = properties_sf$id)

# Extract LSOA11CD values and their coordinates for the nearest LSOA11CDs
nearest_lsoa_coords <- lsoa_weights_high_employment %>%
  filter(LSOA11CD %in% properties_sf$nearest_LSOA11CD) %>%
  mutate(latitude = st_coordinates(geometry)[, 2],
         longitude = st_coordinates(geometry)[, 1]) %>%
  select(LSOA11CD, latitude, longitude)



# Merge origin and destination data
origins_destinations <- origins %>%
  left_join(properties_sf %>% select(id, nearest_LSOA11CD), by = c("postcode_id" = "id")) %>%
  left_join(nearest_lsoa_coords, by = c("nearest_LSOA11CD" = "LSOA11CD"))



# Define the distance calculation function using r5r
calculate_network_distance <- function(origins_destinations, r5r_core) {
  origins <- origins_destinations %>%
    select(id = postcode_id, lon = lon_origin, lat = lat_origin)
  
  destinations <- origins_destinations %>%
    select(id = nearest_LSOA11CD, lon = longitude, lat = latitude)
  
  # Calculate distances
  detailed_routes <- detailed_itineraries(
    r5r_core,
    origins = origins,
    destinations = destinations,
    mode = "CAR",
    verbose = FALSE
  )
  
  # Aggregate distances
  distances <- detailed_routes %>%
    group_by(from_id) %>%
    summarize(distance_to_LSOA = sum(distance))
  
  # Join distances back to origins
  origins_destinations <- origins_destinations %>%
    left_join(distances, by = c("postcode_id" = "from_id"))
  
  return(origins_destinations)
}

# Apply the function to calculate distances
final_combined_data <- calculate_network_distance(origins_destinations, r5r_core)

# Merge the calculated network distances back to the properties_sf data frame
properties_sf <- properties_sf %>%
  left_join(final_combined_data %>% select(postcode_id, distance_to_LSOA), by = c("id" = "postcode_id"))

#how many nas in distance_to_LSOA
sum(is.na(properties_sf$distance_to_LSOA))

######## second distance


# Prepare origins (properties_sf) and destinations (second_nearest LSOA11CD centroids)
origins <- properties_sf %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(lat_origin = Y, lon_origin = X) %>%
  mutate(postcode_id = properties_sf$id)

# Extract coordinates for the second nearest LSOA11CD
second_lsoa_coords <- lsoa_weights_high_employment %>%
  filter(LSOA11CD %in% properties_sf$second_nearest_LSOA11CD) %>%
  mutate(coordinates = st_coordinates(geometry)) %>%
  mutate(latitude = coordinates[, 2], longitude = coordinates[, 1]) %>%
  select(LSOA11CD, latitude, longitude)


# Merge origin and destination data for second nearest LSOA11CD
origins_destinations_second <- origins %>%
  left_join(properties_sf %>% select(id, second_nearest_LSOA11CD), by = c("postcode_id" = "id")) %>%
  left_join(second_lsoa_coords, by = c("second_nearest_LSOA11CD" = "LSOA11CD"))

# Define the distance calculation function using r5r for second nearest LSOA11CD
calculate_network_distance_second <- function(origins_destinations_second, r5r_core) {
  origins <- origins_destinations_second %>%
    select(id = postcode_id, lon = lon_origin, lat = lat_origin)
  
  destinations <- origins_destinations_second %>%
    select(id = second_nearest_LSOA11CD, lon = longitude, lat = latitude)
  
  # Calculate distances
  detailed_routes <- detailed_itineraries(
    r5r_core,
    origins = origins,
    destinations = destinations,
    mode = "CAR",
    verbose = FALSE
  )
  
  # Aggregate distances
  distances <- detailed_routes %>%
    group_by(from_id) %>%
    summarize(distance_to_second_LSOA = sum(distance))
  
  # Join distances back to origins
  origins_destinations_second <- origins_destinations_second %>%
    left_join(distances, by = c("postcode_id" = "from_id"))
  
  return(origins_destinations_second)
}

# Apply the function to calculate distances for second nearest LSOA
final_combined_data_second <- calculate_network_distance_second(origins_destinations_second, r5r_core)

# Merge the calculated network distances back to the properties_sf data frame
properties_sf <- properties_sf %>%
  left_join(final_combined_data_second %>% select(postcode_id, distance_to_second_LSOA), by = c("id" = "postcode_id"))




###third ----------

# Prepare origins (properties_sf) and destinations (third_nearest LSOA11CD centroids)
origins <- properties_sf %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(lat_origin = Y, lon_origin = X) %>%
  mutate(postcode_id = properties_sf$id)

# Extract coordinates for the third nearest LSOA11CD
third_lsoa_coords <- lsoa_weights_high_employment %>%
  filter(LSOA11CD %in% properties_sf$third_nearest_LSOA11CD) %>%
  mutate(coordinates = st_coordinates(geometry)) %>%
  mutate(latitude = coordinates[, 2], longitude = coordinates[, 1]) %>%
  select(LSOA11CD, latitude, longitude)


# Merge origin and destination data for third nearest LSOA11CD
origins_destinations_third <- origins %>%
  left_join(properties_sf %>% select(id, third_nearest_LSOA11CD), by = c("postcode_id" = "id")) %>%
  left_join(third_lsoa_coords, by = c("third_nearest_LSOA11CD" = "LSOA11CD"))

# Define the distance calculation function using r5r for third nearest LSOA11CD
calculate_network_distance_third <- function(origins_destinations_third, r5r_core) {
  origins <- origins_destinations_third %>%
    select(id = postcode_id, lon = lon_origin, lat = lat_origin)
  
  destinations <- origins_destinations_third %>%
    select(id = third_nearest_LSOA11CD, lon = longitude, lat = latitude)
  
  # Calculate distances
  detailed_routes <- detailed_itineraries(
    r5r_core,
    origins = origins,
    destinations = destinations,
    mode = "CAR",
    verbose = FALSE
  )
  
  # Aggregate distances
  distances <- detailed_routes %>%
    group_by(from_id) %>%
    summarize(distance_to_third_LSOA = sum(distance))
  
  # Join distances back to origins
  origins_destinations_third <- origins_destinations_third %>%
    left_join(distances, by = c("postcode_id" = "from_id"))
  
  return(origins_destinations_third)
}

# Apply the function to calculate distances for third nearest LSOA
final_combined_data_third <- calculate_network_distance_third(origins_destinations_third, r5r_core)

# Merge the calculated network distances back to the properties_sf data frame
properties_sf <- properties_sf %>%
  left_join(final_combined_data_third %>% select(postcode_id, distance_to_third_LSOA), by = c("id" = "postcode_id"))


# save properties_sf as rdata
save(properties_sf, file = "Distances/Properties_sf.RData")

min_distance_dataset <- properties_sf %>%
  mutate(
    min_distance = pmin(distance_to_LSOA, distance_to_second_LSOA, distance_to_third_LSOA, na.rm = TRUE)
  ) %>%
  select(min_distance, postcode, id)

# Inspect the new dataframe
head(min_distance_dataset)

#how many nas
sum(is.na(min_distance_dataset$min_distance))
# save min_distance_dataset as rdata
save(min_distance_dataset, file = "Distances/Min_Distance_Dataset_LSOA.RData")

# done min distance to sub centre , looking at 10000 jobs. 84 LSOAs. 






######## DISTANCE ROUTING FOR CBD 

# load libraries
library(r5r)
library(sf)
library(ggplot2)
library(data.table)
library(dplyr)


# load properties_with_census_df
load("properties_with_census_df.RData")

# load lsoa
load("lsoa.RData")

#where is the cbd - what is the highest LSOA11CD in the Employment category
lsoa$Employment <- as.numeric(lsoa$Employment)

# centroids for each lsoa
lsoa_weights <- read.csv("LSOA_Dec_2011_WEIGHTS.csv")

# Convert to sf object with the British National Grid CRS
lsoa_weights_sf <- st_as_sf(lsoa_weights, coords = c("x", "y"), crs = 27700)

# Transform the CRS to WGS 84 (EPSG:4326)
lsoa_weights_sf <- st_transform(lsoa_weights_sf, crs = 4326)


# find highest row - this is CBD / destination 
highest_employment_lsoa <- lsoa[which.max(lsoa$Employment), ]

#join lsoa_weights_sf to highest_employment_lsoa
highest_employment_lsoa <- highest_employment_lsoa %>%
  left_join(lsoa_weights_sf, by = c("LSOA11CD" = "LSOA11CD"))

# create a travel time matrix from properties to CBD


# increase Java memory
options(java.parameters = "-Xmx14G")

# Define R5R directory
r5r_dir <- 'C:/Users/aoliv/OneDrive - University of Glasgow/Diss - Second draft/complete_router'

# Set up the network
r5r_core <- setup_r5(data_path = r5r_dir, verbose = FALSE)

mode <- c("WALK", "TRANSIT")
#use 20240929 - strong amount of schedules this day - not really looking at frequency here just travel time
# departure time
departure_datetime <- as.POSIXct("29-09-2024 08:00:00", 
                                 format = "%d-%m-%Y %H:%M:%S")

#origins is every postcode 
#make properties_with_census sf
properties_sf <- st_as_sf(properties_with_census_df, crs = 4326)


#rename id.x to id 
properties_sf <- properties_sf %>%
  rename(id = id.x)

#drop geometry.y
properties_sf <- properties_sf %>%
  select(-geometry.y)

#rename geometry.x to geometry
properties_sf <- properties_sf %>%
  rename(geometry = geometry.x)

#remove duplicates in postcode column
properties_sf <- properties_sf %>%
  distinct(postcode, geometry, .keep_all = TRUE)

#rename id.x to id
#properties_sf <- properties_sf %>%
#  rename(id = id.x)

# Prepare origins (properties) and destinations (nearest LSOA11CD) - ensure id, postcode are kept
origins <- properties_sf %>%
  st_coordinates() %>%
  as.data.frame() %>%
  rename(lat = Y, lon = X) %>%
  mutate(id = as.character(properties_sf$id), # ensure id is character
         postcode = properties_sf$postcode) %>%
  select(id, postcode, lat, lon)

# Destination is highest_employment_lsoa
destination <- highest_employment_lsoa %>%
  mutate(id = as.character(LSOA11CD), # add id column as character
         lat = st_coordinates(geometry)[, 2],
         lon = st_coordinates(geometry)[, 1]) %>%
  select(id, lat, lon)

max_walk_time <- 60

# check CRS of origins and destinations
st_crs(origins)
st_crs(destination)

#set as sf and crs at 4326
origins <- st_as_sf(origins, coords = c("lon", "lat"), crs = 4326)
destination <- st_as_sf(destination, coords = c("lon", "lat"), crs = 4326)


# Generate detailed itineraries
det <- detailed_itineraries(r5r_core = r5r_core,
                            origins = origins,
                            destinations = destination,
                            mode = "CAR",
                            departure_datetime = departure_datetime,
                            max_walk_time = max_walk_time,
                            shortest_path = FALSE)

#drop route from det
det <- det %>%
  select(-route)

# join origins to det by id
det <- det %>%
  left_join(origins, by = c("from_id" = "id"))
# Define batch sizes
batch_size <- 5000

#Save det
save(det, file = "Distances/Detailed_Itineraries_CBD.RData")

ttm <- travel_time_matrix(r5r_core,   
                          origins = destination,
                          destinations = origins,    
                          mode = "CAR",
                          max_trip_duration = 300,
                          departure_datetime = departure_datetime)

head(ttm, n = 10)

# join origins to ttm by id
ttm <- ttm %>%
  left_join(origins, by = c("to_id" = "id"))#



# properties_ttm
  #drop from_id, to_id, lat, lon 
properties_ttm <- ttm %>%
  as.data.frame() %>%
  select(-from_id, -to_id, -lat, -lon)
#save properties_ttm
save(properties_ttm, file = "Distances/Properties_TTM_CBD.RData")


#load postcode_sf
load("Postcode_sf.RData")



library(ggplot2)
library(ggspatial)
library(sf)
library(prettymapr)




# Assuming postcode_sf and ttm are already defined and processed
library(ggplot2)
library(ggspatial)
library(sf)
library(prettymapr)

# Define the point you want to add as an sf object
cbd_point <- st_as_sf(data.frame(lon = -0.09480062, lat = 51.51144), 
                      coords = c("lon", "lat"), crs = 4326)

# Join postcode data with travel time data
postcode_sf <- postcode_sf %>%
  left_join(ttm, by = c("POSTCODE" = "postcode"))

# Calculate the specific breaks at 20%, 40%, 60%, 80% of the range for travel time
min_time <- min(postcode_sf$travel_time_p50, na.rm = TRUE)
max_time <- max(postcode_sf$travel_time_p50, na.rm = TRUE)

# Define the breakpoints
breaks <- c(min_time,
            min_time + 0.2 * (max_time - min_time),
            min_time + 0.4 * (max_time - min_time),
            min_time + 0.6 * (max_time - min_time),
            min_time + 0.8 * (max_time - min_time),
            max_time)

# Create quantiles based on these breaks
postcode_sf <- postcode_sf %>%
  mutate(time_quantile = cut(travel_time_p50, breaks = breaks, include.lowest = TRUE))

# Create labels for the quantiles
quantile_labels <- paste0(round(breaks[-6], 2), " - ", round(breaks[-1], 2))
#how many na in travel_time_p50 in postcode_sf
sum(is.na(postcode_sf$travel_time_p50))

#remove na 
postcode_sf <- postcode_sf %>%
  filter(!is.na(travel_time_p50))
# Plot the map with specified quantile ranges
ggplot(data = postcode_sf) +
  annotation_map_tile(type = "cartolight", zoom = NULL) +  # Add basemap
  geom_sf(aes(fill = time_quantile), color = NA) +  # Fill map with quantile categories
  geom_sf(data = cbd_point, aes(color = "Central Business District"), size = 3) +  # Highlight CBD
  scale_fill_viridis_d(direction = -1, option = "plasma", na.value = "gray90", name = "Travel Time (Quantiles)",
                       labels = quantile_labels) +  # Discrete color scale with quantile ranges
  scale_color_manual(values = "red", name = NULL) +  # Custom legend entry for the CBD point
  annotation_north_arrow(location = "tl", which_north = "true",  # North arrow
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) +  # Add north arrow at top left
  coord_sf(expand = FALSE) +  # Maintain correct coordinate system
  theme_minimal() +  # Use a minimal theme
  theme(
    legend.text = element_text(face = "bold"),  # Make the legend text bold
    legend.spacing.y = unit(0.5, 'cm'),  # Adjust the vertical spacing between legend items
    panel.grid.major = element_line(color = "gray90"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )
