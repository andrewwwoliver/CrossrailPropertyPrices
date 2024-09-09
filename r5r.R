
# Allocate RAM to Java 
options(java.parameters = "-Xmx16G")
# Load R5R
library(r5r)
# Build/Load network ------------------------------------------------------


# Define R5R directory
r5r_dir <- 'complete_router'

# Create directory to store input data and network
dir.create(r5r_dir)
# Get input files
input_files <- list.files(
  'data', 
  recursive = TRUE,
  pattern = 'gtfs\\.zip$|pbf$',
  full.names = TRUE
)
# List input files
input_files
# Copy input files
lapply(input_files, function(x)
  file.copy(x, paste0(r5r_dir, '/', basename(x)))
)

# Build/read multimodal transport network
# Indicate the path where OSM and GTFS data are stored
# This may take few minutes
r5r_core <- setup_r5(data_path = r5r_dir, verbose = TRUE)

gc(reset = TRUE)



# Load necessary libraries
library(r5r)
library(sf)
library(tidyverse)

# Ensure r5r_core is properly initialized
if (is.null(r5r_core)) {
  stop("Error: r5r_core is not properly initialized. Please ensure the setup_r5 function ran successfully.")
}

# Points of origin and destination
abbey_wood <- data.frame(id = "abbey_wood", lat = 51.49123, lon = -0.120840)
acton <- data.frame(id = "acton", lat = 51.51675, lon = -0.26968)

# Routing inputs
mode <- c("WALK", "TRANSIT")
max_walk_time <- 15
max_trip_duration <- 120L
walk_speed <- 4.8
departure_datetime <- as.POSIXct("22-11-2021 07:00:00", format = "%d-%m-%Y %H:%M:%S", tz = 'Europe/London')

# Check the data frames and parameters
if (any(is.na(abbey_wood)) || any(is.na(acton))) {
  stop("Error: Check the origin and destination data frames for missing values.")
}

if (is.null(departure_datetime)) {
  stop("Error: Departure datetime is not set.")
}

# Debug: Print parameters to ensure they are correctly set
print(list(
  r5r_core = r5r_core,
  origins = abbey_wood,
  destinations = acton,
  mode = mode,
  departure_datetime = departure_datetime,
  max_trip_duration = max_trip_duration,
  walk_speed = walk_speed,
  max_walk_time = max_walk_time
))

# Estimate fastest PT route - fixed departure time
pt_route <- tryCatch({
  detailed_itineraries(
    r5r_core = r5r_core, 
    origins = abbey_wood, 
    destinations = acton,
    mode = mode,
    departure_datetime = departure_datetime, 
    max_trip_duration = max_trip_duration, 
    walk_speed = walk_speed, 
    max_walk_time = max_walk_time
  )
}, error = function(e) {
  stop("Error in detailed_itineraries function: ", e$message)
})

# Check if pt_route is returned successfully
if (is.null(pt_route)) {
  stop("Error: The detailed_itineraries function returned NULL. Check the parameters and setup.")
}

# View the route
print(pt_route)
