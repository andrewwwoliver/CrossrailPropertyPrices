# Load necessary libraries
library(data.table)

# Load stop_times.txt
stop_times <- fread("rail_router/london_rail.gtfs/stop_times.txt")

# Load crossrail_stops_gtfs.csv
crossrail_stops <- fread("data/crossrail_stops_gtfs.csv")

# Filter stop_times by stop_id in crossrail_stops
filtered_stop_times <- stop_times[stop_id %in% crossrail_stops$stop_id]

# link stop_name from crossrail_stops by stop_id - just stop_name
filtered_stop_times <- merge(filtered_stop_times, crossrail_stops[, .(stop_id, stop_name)], by = "stop_id", all.x = TRUE)

# Write filtered stop_times to a new file
#fwrite(filtered_stop_times, "filtered_stop_times.csv")

# Load trips.txt
trips <- fread("rail_router/london_rail.gtfs/trips.txt")
# Merge filtered_stop_times with trips by trip_id
merged_data <- merge(filtered_stop_times, trips, by = "trip_id", all.x = TRUE)

# View the first few rows to check the result
head(merged_data)


# Assuming the file is in the same directory as your script
calendar <- fread("rail_router/london_rail.gtfs/calendar.txt")

merged_data <- merge(merged_data, calendar, by = "service_id", all.x = TRUE)

# View the first few rows to check the result
head(merged_data)


# routes
#stops df, keep stop_id, stop_code, stop_name
stops <- crossrail_stops[, .(stop_id, stop_code, stop_name)]

#drop 'Rail Station' from any entries in stop_name
stops$stop_name <- gsub(" Rail Station", "", stops$stop_name)


# Step 1: Load the routes.txt file
routes <- fread("rail_router/london_rail.gtfs/routes.txt")

# Step 2: Separate route_long_name into route_from and route_to
# First, remove "From " and split by " to "
routes[, route_from := sub("From ", "", gsub(" to.*", "", route_long_name))]
routes[, route_to := sub(".* to ", "", route_long_name)]


# Step 2: Filter routes based on stop names
filtered_routes <- routes[
  route_from %in% stops$stop_name & 
    route_to %in% stops$stop_name
]

#filter for crossrail routes
merged_data <- merged_data[route_id %in% filtered_routes$route_id]

# Step 2: View the filtered result
head(merged_data)


# what is the range in start_dATE
range(merged_data$start_date)

#what start_date has the most entries
merged_data[, .N, by = start_date][order(-N)]



#count the number of entries per stop id
stops_all_time <- merged_data[, .N, by = stop_name]

# Assuming stops_all_time is the DataFrame containing the stop data

# Create a new column 'score' based on the conditions
stops_all_time[, crossrail_frequency := fifelse(N >= 10000, 10,
                                  fifelse(N >= 9000, 9,
                                          fifelse(N >= 8000, 8,
                                                  fifelse(N >= 7000, 7,
                                                          fifelse(N >= 6000, 6,
                                                                  fifelse(N >= 5000, 5,
                                                                          fifelse(N >= 4000, 4,
                                                                                  fifelse(N >= 3000, 3,
                                                                                          fifelse(N >= 2000, 2, 1)))))))))]
# View the updated DataFrame
head(stops_all_time)

# Create a named vector with the original stop names and their corresponding new names
stop_name_mapping <- c(
  "WOOLWICH" = "Woolwich Crossrail Station",
  "WHITECHAPEL CROSSRAIL" = "Whitechapel Station",
  "West Ealing Rail Station" = "West Ealing Station",
  "West Drayton Rail Station" = "West Drayton Station",
  "Twyford Rail Station" = "Twyford Rail Station",
  "TOTTENHAM COURT ROAD" = "Tottenham Court Road Station",
  "Taplow Rail Station" = "Taplow Rail Station",
  "Stratford (London) Rail Station" = "Stratford Station",
  "Southall Rail Station" = "Southall Station",
  "Slough Rail Station" = "Slough Rail Station",
  "Shenfield Rail Station" = "Shenfield Rail Station",
  "Seven Kings Rail Station" = "Seven Kings",
  "Romford Rail Station" = "Romford Station",
  "Reading Rail Station" = "Reading Rail Station",
  "PADDINGTON CROSSRAIL" = "Paddington Rail Station",
  "Maryland Rail Station" = "Maryland Station",
  "Manor Park Rail Station" = "Manor Park Rail Station",
  "Maidenhead Rail Station" = "Maidenhead Rail Station",
  "LIVERPOOL ST CROSSRAIL" = "Liverpool Street Station",
  "Langley (Berks) Rail Station" = "Langley (Berks) Rail Station",
  "Iver Rail Station" = "Iver Rail Station",
  "Ilford Rail Station" = "Ilford",
  "Heathrow Terminals 1-3 Rail Station" = "Heathrow Terminals 2 & 3 Rail Station",
  "Heathrow Terminal 5 Rail Station" = "Heathrow Terminal 5",
  "Heathrow Terminal 4 Rail Station" = "Heathrow Terminal 4",
  "Hayes & Harlington Rail Station" = "Hayes & Harlington",
  "Harold Wood Rail Station" = "Harold Wood",
  "Hanwell Rail Station" = "Hanwell",
  "Goodmayes Rail Station" = "Goodmayes",
  "Gidea Park Rail Station" = "Gidea Park",
  "Forest Gate Rail Station" = "Forest Gate Station",
  "FARRINGDON CROSSRAIL" = "Farringdon",
  "Ealing Broadway Rail Station" = "Ealing Broadway",
  "CUSTOM HOUSE" = "Custom House",
  "Chadwell Heath Rail Station" = "Chadwell Heath",
  "CANARY WHARF" = "Canary Wharf Crossrail Station",
  "Burnham (Berks) Rail Station" = "Burnham (Berks) Rail Station",
  "Brentwood Rail Station" = "Brentwood Rail Station",
  "BOND STREET" = "Bond Street Station",
  "Acton Main Line Rail Station" = "Acton Main Line",
  "ABBEY WOOD (CROSSRAIL)" = "Abbey Wood Station"
)

# Rename the stops in the stops_all_time dataframe
stops_all_time$stop_name <- stop_name_mapping[stops_all_time$stop_name]

# View the first few rows to check the result
head(stops_all_time)


# Save stops_all_time as RData
save(stops_all_time, file = "stops_all_time.RData")



##########



