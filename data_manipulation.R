# Load the data
data <- read.csv("pp-complete.csv")

# Print the first few rows of the data
head(data)

# Add headers to the data
colnames(data) <- c("id", "price", "date", "postcode", "property_type", "old_new", "duration", "paon", "saon", "street", "locality", "city", "district", "county", "ppd", "record_status")


# Filter the main dataset to keep only the specified counties
#counties_to_keep <- c("GREATER LONDON", "WEST BERKSHIRE", "ESSEX", "SURREY", 
#                      "WOKINGHAM", "BUCKINGHAMSHIRE", "READING", "OXFORDSHIRE", 
#                      "THURROCK", "WINDSOR AND MAIDENHEAD", "BRACKNELL FOREST", 
#                      "SLOUGH")

#data <- subset(data, county %in% counties_to_keep)

data$date <- as.POSIXct(data$date, format="%Y-%m-%d %H:%M")

# Extract the year from the date column
data$year <- as.numeric(format(data$date, "%Y"))

# Filter the full data to remove listings where the year is 2006 or before
data <- subset(data, year > 2009)

# Print the first few rows of the filtered data
head(data)

#districts_to_include <- c(
#  "WEST BERKSHIRE", "WYCOMBE", "TOWER HAMLETS", "CITY OF WESTMINSTER", "WOKINGHAM",
# "HOUNSLOW", "BEXLEY", "NEWHAM", "REDBRIDGE", "BRENT", "HILLINGDON", "GREENWICH",
#"READING", "HAMMERSMITH AND FULHAM", "SPELTHORNE", "EALING", "SOUTHWARK", 
#"HAVERING", "BARKING AND DAGENHAM", "WINDSOR AND MAIDENHEAD", "BRACKNELL FOREST", 
#"SOUTH OXFORDSHIRE", "KENSINGTON AND CHELSEA", "SLOUGH", "BRENTWOOD", "CITY OF LONDON"
#)

# Filter the dataset to only include the specified districts
#data <- subset(data, district %in% districts_to_include)

# Remove all entries with L in the duration column
#data <- subset(data, duration != "L")

#save as rdata
#save(data, file = "cleaned_data.RData")

# list the areas in district
#unique(data$district)

# how many times does each variable appear in duration
#table(data$duration)

# list the areas in city
#unique(data$city)

#cities_to_include <- c("READING", "BEXLEYHEATH", "LONDON", "WINDSOR", "STAINES", "BRENTWOOD", 
#                       "SLOUGH", "ILFORD", "HORNCHURCH", "HOUNSLOW", "ROMFORD", "BELVEDERE", 
#                      "GREENFORD", "WELLING", "BARKING", "ASHFORD", "MAIDENHEAD", "SOUTHALL", 
#                     "WEST DRAYTON", "DAGENHAM", "HAYES", "FELTHAM", "ERITH", "INGATESTONE", 
#                    "STAINES-UPON-THAMES", "IVER", "CHELMSFORD")

# Filter the dataset to only include the specified cities
#data <- subset(data, city %in% cities_to_include)



#how many rows in data dont have postcode
sum(is.na(data$postcode))

# Load ukpostcodes.csv
uk_postcodes <- read.csv("ukpostcodes.csv")

filtered_data <- data
# link the postcodes to filtered_data by postcode - linking longitude and latitude (drop id from postcode)
filtered_data <- merge(filtered_data, uk_postcodes, by="postcode")

#this reduced data from 351827 to 350583

# Crossrail stops
library(httr)
library(jsonlite)

# Define the API URL
url <- "https://api.tfl.gov.uk/StopPoint/Mode/elizabeth-line"

# Send GET request to the API
response <- GET(url)

# Check the status of the response
if (status_code(response) == 200) {
  # Parse the content of the response
  data_stops <- content(response, "text", encoding = "UTF-8")
  data_json <- fromJSON(data_stops, flatten = TRUE)
  
  # Extract the relevant information
  stops <- data_json$stopPoints
  stops_info <- data.frame(
    id = stops$id,
    name = stops$commonName,
    lat = stops$lat,
    lon = stops$long
  )
  
  # Print the first few rows of the stops information
  head(stops_info)
  
} else {
  print("Failed to fetch data from TFL API")
}
# rename long to longitude and lat to latitude
stops_info <- stops_info %>%
  rename(longitude = lon, latitude = lat)

stops_unique <- stops_info %>%
  distinct(name, .keep_all = TRUE)

stops_unique <- stops_unique %>%
  slice(1:41)

#save stops_unique as rdata
save(stops_unique, file = "stops_unique.RData")
# load stops_unique
load("stops_unique.RData")

# Load necessary library
library(geosphere)

# Function to calculate the nearest distance and nearest station
calculate_nearest <- function(latitude, longitude, stops) {
  distances <- distGeo(matrix(c(longitude, latitude), nrow = 1), stops[, c("longitude", "latitude")])
  min_index <- which.min(distances)
  list(distance = distances[min_index], station = stops$name[min_index])
}

# Apply the function to each row in filtered_data
nearest_info <- apply(filtered_data, 1, function(row) {
  result <- calculate_nearest(as.numeric(row["latitude"]), as.numeric(row["longitude"]), stops_unique)
  c(result$distance, result$station)
})

# Convert the result to a data frame and add it to filtered_data
nearest_info_df <- data.frame(matrix(unlist(nearest_info), ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
names(nearest_info_df) <- c("nearest_distance", "nearest_station")
filtered_data <- cbind(filtered_data, nearest_info_df)

#frequency of each station in nearest_info_df
table(nearest_info_df$nearest_station)

# Convert nearest_distance to numeric
filtered_data$nearest_distance <- as.numeric(filtered_data$nearest_distance)

all_data <- filtered_data

tenk_data <- subset(all_data, nearest_distance < 10000)

#save tenk
save(tenk_data, file = "tenk_data.RData")

# load tenk data
load("tenk_data.RData")

#how many rows have nearest_distance as 1500 or more
sum(tenk_data$nearest_distance >= 2500)

# remove any rows with nearest_distance as 1500 or more
filtered_data <- subset(tenk_data, nearest_distance < 2500)

# test_data = distance 2500 to 3000
test_data <- subset(tenk_data, nearest_distance >= 2500 & nearest_distance < 3000)

#link identifying data - criminal, school, hospital, employment,
# cbd, national rail, london underground, airport, density, greenspace

# caz
# Load necessary libraries
library(sf)
library(dplyr)

# Load the CAZ data from the GeoJSON file
caz_geojson_url <- "https://files.planning.data.gov.uk/dataset/central-activities-zone.geojson"
caz_data <- st_read(caz_geojson_url)

# Ensure the geometries in CAZ data are valid
caz_data <- st_make_valid(caz_data)

# Ensure the properties data is in the correct format
properties <- filtered_data %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Calculate the nearest distance to the CAZ for each property
distances_to_caz <- st_distance(properties, caz_data)

# Convert distances to numeric and find the minimum distance for each property
properties$min_distance_to_caz <- apply(distances_to_caz, 1, function(x) min(as.numeric(x)))

# Convert properties back to a dataframe
properties_df <- as.data.frame(properties)

# Print the first few rows to verify
head(properties_df)

# Load all tfl stations
tfl_stops <- read.csv("Stations_20180921.csv")

#rename x and y longitude and latitude
tfl_stops <- tfl_stops %>%
  rename(longitude = x, latitude = y)

# what is the frequency of each variable in network
table(tfl_stops$NETWORK)

# Filter tfl into London Underground vs. DLR / Overground / Tramlink
underground_stops <- tfl_stops %>%
  filter(NETWORK == "London Underground")

overground_stops <- tfl_stops %>%
  filter(NETWORK %in% c("DLR", "London Overground", "Tramlink"))

# Load rail stations
rail_stations <- read.csv("rail_stations.csv")
# rename lat and long
rail_stations <- rail_stations %>%
  rename(longitude = long, latitude = lat)

# Convert stops data frames to spatial objects
underground_stops_sf <- underground_stops %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

overground_stops_sf <- overground_stops %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

rail_stations_sf <- rail_stations %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

# Ensure the geometries in the stops data are valid
underground_stops_sf <- st_make_valid(underground_stops_sf)
overground_stops_sf <- st_make_valid(overground_stops_sf)
rail_stations_sf <- st_make_valid(rail_stations_sf)

# Calculate the nearest distance to the Underground stops for each property
distances_to_underground <- st_distance(properties, underground_stops_sf)
properties$underground_distance <- apply(distances_to_underground, 1, function(x) min(as.numeric(x)))

# Calculate the nearest distance to the Overground stops for each property
distances_to_overground <- st_distance(properties, overground_stops_sf)
properties$overground_distance <- apply(distances_to_overground, 1, function(x) min(as.numeric(x)))

library(sf)

# Define the bounding box coordinates
xmin <- -2.085205
ymin <- 50.622238
xmax <- 1.529297
ymax <- 52.070788

# Create a bounding box polygon
bbox <- st_polygon(list(matrix(c(xmin, ymin, 
                                 xmax, ymin, 
                                 xmax, ymax, 
                                 xmin, ymax, 
                                 xmin, ymin), 
                               ncol = 2, byrow = TRUE)))

# Convert the bbox to an sf object
bbox_sf <- st_sfc(bbox, crs = st_crs(rail_stations_sf))

# Filter the rail stations to those within the bounding box
rail_stations_sf <- st_intersection(rail_stations_sf, bbox_sf)


# Calculate the nearest distance to the Rail stations for each property
distances_to_rail <- st_distance(properties, rail_stations_sf)
properties$rail_distance <- apply(distances_to_rail, 1, function(x) min(as.numeric(x)))

# Airport:

# Define the coordinates for Heathrow Airport
heathrow_coords <- c(-0.454295, 51.470020)

# Calculate the distance to Heathrow for each property
properties$distance_heathrow <- apply(st_coordinates(properties), 1, function(coords) {
  distGeo(coords, heathrow_coords)
})

# Convert properties back to a dataframe
properties_df <- as.data.frame(properties)

# criminal, school, hospital, employment, density , greenspace

# Load necessary libraries
library(dplyr)
library(sf)
library(readr)



# join census data to properties data
# Ensure the census data is a spatial dataframe

load("census_data.RData")

# convert census_data to sf
census_data_sf <- st_as_sf((census_data), crs = 4326)

# Ensure both datasets are in the same CRS
properties <- st_transform(properties, crs = st_crs(census_data_sf))

# Make the geometries valid
census_data_sf <- st_make_valid(census_data_sf)
properties <- st_make_valid(properties)

# Perform the spatial join
properties_with_census <- st_join(properties, census_data_sf, join = st_within)

# Convert back to a dataframe if needed
properties_with_census_df <- as.data.frame(properties_with_census)

# Print the first few rows to verify
head(properties_with_census_df)


#binaries

# Convert old_new to binary variable
properties_with_census_df$old_new_binary <- ifelse(properties_with_census_df$old_new == "N", 1, 0)

# Create binary variables for property_type
properties_with_census_df$detached_binary <- ifelse(properties_with_census_df$property_type == "D", 1, 0)
properties_with_census_df$terraced_binary <- ifelse(properties_with_census_df$property_type == "T", 1, 0)
properties_with_census_df$flat_binary <- ifelse(properties_with_census_df$property_type == "F", 1, 0)
properties_with_census_df$semi_detached_binary <- ifelse(properties_with_census_df$property_type == "S", 1, 0)

# Print the first few rows to verify
head(properties_with_census_df)

#rename nearest_distance nearest_crossrail
properties_with_census_df <- properties_with_census_df %>%
  rename(nearest_crossrail = nearest_distance)

#check for msising values in properties_with_census
sum(is.na(properties_with_census_df))

#where are the missing values
colSums(is.na(properties_with_census_df))

# make a subset with the missing values
missing_subset <- properties_with_census_df %>%
  filter(is.na(underground_distance) | is.na(population_density) | is.na(employment_rate) | 
           is.na(percent_households_deprived) | is.na(detached_binary) | is.na(terraced_binary) | 
           is.na(flat_binary))

# remove the missing values from properties_with_census
properties_with_census_df <- properties_with_census_df %>%
  filter(!is.na(underground_distance) & !is.na(population_density) & !is.na(employment_rate) & 
           !is.na(percent_households_deprived) & !is.na(detached_binary) & !is.na(terraced_binary) & 
           !is.na(flat_binary))

# join lsoa to properties_with_census_df by LSOA11CD column in each
# load lsoa
load("lsoa.RData")

# link by LSOA11CD column in each
properties_with_census_df <- merge(properties_with_census_df, lsoa, by = "LSOA11CD")

# link "Distances/Min_Distance_Dataset_LSOA.RData" to each by postcode
load("Distances/Min_Distance_Dataset_LSOA.RData")

#join to df by postcode
properties_with_census_df <- merge(properties_with_census_df, min_distance_dataset, by = "postcode")

#how many nas in min_distance
sum(is.na(properties_with_census_df$min_distance))

#rename min_distance to distance_subcentre
properties_with_census_df <- properties_with_census_df %>%
  rename(distance_subcentre = min_distance)

#drop any rows with na in distance_subcentre
properties_with_census_df <- properties_with_census_df %>%
  filter(!is.na(distance_subcentre))
#drop id column
properties_with_census_df <- properties_with_census_df %>%
  select(-id)

#load properties
load("Distances/Properties_TTM_CBD.RData")

#join properties_ttm to properties_with_census_df by postcode
properties_with_census_df <- merge(properties_with_census_df, properties_ttm, by = "postcode")

#load noise data
load("noise_properties_sf.RData")

#merge by id.x
properties_with_census_df <- merge(properties_with_census_df, noise_properties_sf, by = "id.x")

#load crime_count_by_lsoa
load("crime_count_by_lsoa.rdata")

#merge by LSOA11CD
properties_with_census_df <- merge(properties_with_census_df, crime_count_by_lsoa, by = "LSOA11CD")

#load CPI
load("CPI.RData")

#link by year 
properties_with_census_df <- merge(properties_with_census_df, cpi, by = "year")

#load greenspace
load("greenspace.RData")

#merge by LSOA11CD
properties_with_census_df <- merge(properties_with_census_df, greenspace, by = "LSOA11CD")

#save properties_with_census_df as rdata
#save(properties_with_census_df, file = "properties_with_census_df.RData")

# load properties_with_census_df
load("properties_with_census_df.RData")


# Load necessary libraries
library(dplyr)
library(geosphere)
library(dplyr)
library(sf)
library(readr)
library(car)


properties_with_census <- properties_with_census_df



# Determine the thresholds for extreme values
lower_bound <- quantile(properties_with_census$price, 0.05)
upper_bound <- quantile(properties_with_census$price, 0.95)

# Filter the data to remove extreme values
properties_with_census <- properties_with_census %>%
  filter(price >= lower_bound & price <= upper_bound)


#filter to remove dates after end of 2023
#station names in properties with census
unique(properties_with_census$nearest_station)

#make a subset where nearest_station is Reading Wwyford , Taplow , Burnham , Slough , Langley , Iver or West Drayton
west_crossrail_stations <- c("Reading Rail Station", "Twyford Rail Station", "Taplow Rail Station", "Burnham (Berks) Rail Station", 
                             "Slough Rail Station", "Langley (Berks) Rail Station", "Iver Rail Station", "West Drayton Station",
                             "Heathrow Terminal 4", "Heathrow Terminal 5", "Heathrow Terminals 2 & 3 Rail Station", "Haayes & Harlington",
                             "Hanwell", "Southall Station", "Hanwell", "West Ealing Station", "Ealing Broading", "Acton Main Line")

# make a subset for east stations - Stratford, Maryland, Forest Gate, Manor Park, Ilford, Seven Kings, Goodmayes, Chadwell Heath, Romford, Gidea Park, Harold Wood, Brentwood
# names of stations:
east_crossrail_stations <- c("Stratford Station", "Maryland Station", "Forest Gate Station", "Manor Park Rail Station", 
                             "Ilford", "Seven Kings", "Goodmayes", "Chadwell Heath", 
                             "Romford Station", "Gidea Park", "Harold Wood", "Brentwood Rail Station", 
                             "Woolwich Crossrail Station", "Abbey Wood Station")

central_crossrail_stations <- c("Whitechapel Station", "Bond Street Station", "Tottenham Court Road Station", "Farringdon",
                                "Liverpool Street Station", "Whitechapel Station", "Canary Wharf Station", "Custom House")

# nanmes in nearest_Station 
unique(properties_with_census$nearest_station)

#remove rows with O in property_type
properties_with_census <- properties_with_census %>%
  filter(property_type != "O")

#remove id.y and rename id.x to id
properties_with_census <- properties_with_census %>%
  select(-id.y) %>%
  rename(id = id.x)
# join r5r distances to properties_with_census

# load min_distance_dataset
load("Distances/Min_Distance_Dataset.RData")


# join road_distance_crossrail and nearest_crossrail_network by id 
properties_with_census <- merge(properties_with_census, min_distance_dataset, by = "id")



# analysis!


# Define the distance threshold for being "close" to a Crossrail stop (e.g., 1000 meters)
distance_threshold <- 1000


# Create a binary variable for treatment (close to Crossrail stop)
properties_with_census <- properties_with_census %>%
  mutate(close_to_crossrail = ifelse(nearest_crossrail <= distance_threshold, 1, 0))

# Create a binary variable for network treatment (close to Crossrail stop)
properties_with_census <- properties_with_census %>%
  mutate(close_to_crossrail_network = ifelse(road_distance_crossrail <= distance_threshold, 1, 0))


# Define the pre-treatment and post-treatment periods (example years, adjust based on actual data)
crossrail_start_year <- 2021
properties_with_census <- properties_with_census %>%
  mutate(post_treatment = ifelse(year >= crossrail_start_year, 1, 0))

# Create the interaction term
properties_with_census <- properties_with_census %>%
  mutate(treatment_post = close_to_crossrail * post_treatment)

# interaction network
properties_with_census <- properties_with_census %>%
  mutate(treatment_post_network = close_to_crossrail_network * post_treatment)

# Apply log transformation, handling zero or negative values
properties_with_census <- properties_with_census %>%
  mutate(log_price = log(price),
         log_min_distance_to_caz = log(min_distance_to_caz + 1),  # Adding 1 to avoid log(0)
         log_underground_distance = log(underground_distance + 1),
         log_distance_heathrow = log(distance_heathrow + 1),
         log_population_density = log(population_density + 1),
         log_subcentre_distance = log(distance_subcentre + 1))

# convert nearest_crossrail from metres to kilometres
properties_with_census$nearest_crossrail <- properties_with_census$nearest_crossrail / 1000

#convert road_distance_crossrail from metres to kilometres
properties_with_census$road_distance_crossrail <- properties_with_census$road_distance_crossrail / 1000


# subset properties_with_sensus where nearest_station is in subset_crossrail_stations
western_subset <- properties_with_census %>%
  filter(nearest_station %in% west_crossrail_stations)

# eastern subset
eastern_subset <- properties_with_census %>%
  filter(nearest_station %in% east_crossrail_stations)

# central subset
central_subset <- properties_with_census %>%
  filter(nearest_station %in% central_crossrail_stations)

# make summary stats dataset
summary_data <- properties_with_census



center_predictors <- function(data, predictors, means) {
  centered_data <- data
  for (var in predictors) {
    centered_data[[var]] <- data[[var]] - means[[var]]
  }
  return(centered_data)
}


# non-centred datasets
non_centred_properties_with_census <- properties_with_census
non_centred_western_subset <- western_subset
non_centred_eastern_subset <- eastern_subset
non_centred_central_subset <- central_subset

#summary table

# Calculate the mean of each predictor for the full dataset
predictors <- c("close_to_crossrail_network", "post_treatment", "log_underground_distance", 
                "log_population_density", "log_min_distance_to_caz", 
                "semi_detached_binary", "flat_binary", "avg_bedrooms", 
                "average_qualification_level_4")

means <- sapply(properties_with_census[predictors], mean, na.rm = TRUE)

# Center the predictors for all datasets
properties_with_census <- center_predictors(properties_with_census, predictors, means)
western_subset <- center_predictors(western_subset, predictors, means)
eastern_subset <- center_predictors(eastern_subset, predictors, means)
central_subset <- center_predictors(central_subset, predictors, means)


# Run the DiD regression model
did_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                  log_underground_distance  +  log_population_density  + 
                  log_min_distance_to_caz +  semi_detached_binary + detached_binary + 
                  flat_binary + avg_bedrooms + average_qualification_level_4,
                data = properties_with_census)

# Summarize the model
summary(did_model)

# Run the DiD regression model
did_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                  log_underground_distance  +  log_population_density  + 
                  log_min_distance_to_caz +  semi_detached_binary + detached_binary + 
                  flat_binary + avg_bedrooms + average_qualification_level_4,
                data = properties_with_census)

# Summarize the model
summary(did_model)

library(lme4)

# Run the multilevel regression model
multilevel_model <- lmer(log_price ~ close_to_crossrail_network * post_treatment +
                           log_underground_distance + log_population_density + 
                           log_min_distance_to_caz + semi_detached_binary + 
                           detached_binary + flat_binary + avg_bedrooms + 
                           average_qualification_level_4 + 
                           (1 | OA21CD),
                         data = properties_with_census)

# Summarize the model
summary(multilevel_model)

# Check VIF
vif(did_model, type = "predictor")
library(lmtest)

# Perform the Durbin-Watson test
dw_test <- dwtest(did_model)
print(dw_test)

library(AER)

properties_with_census %>% lm(log_price ~ close_to_crossrail_network * post_treatment +
                                log_underground_distance  +  log_population_density  + 
                                log_min_distance_to_caz +  semi_detached_binary + detached_binary + 
                                flat_binary + avg_bedrooms + average_qualification_level_4, data = . ) %>% plot(which=1) 

# western london model
# Run the DiD regression model
western_did_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                          log_underground_distance  +  log_population_density  + 
                          log_min_distance_to_caz +  semi_detached_binary + detached_binary + 
                          flat_binary + avg_bedrooms + average_qualification_level_4,
                        data =  western_subset)

# Summarize the model
summary(western_did_model)

#eastern london model
# Run the DiD regression model
eastern_did_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                          log_underground_distance  +  log_population_density  + 
                          log_min_distance_to_caz +  semi_detached_binary + detached_binary + 
                          flat_binary + avg_bedrooms + average_qualification_level_4,
                        data =  eastern_subset)

# Summarize the model
summary(eastern_did_model)

#central london model
# Run the DiD regression model
central_did_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                          log_underground_distance  +  log_population_density  + 
                          log_min_distance_to_caz +  semi_detached_binary + detached_binary + 
                          flat_binary + avg_bedrooms + average_qualification_level_4,
                        data =  central_subset)

# Summarize the model
summary(central_did_model)


library(lmtest)
library(sandwich)
# could put in appendixes - is it needed? the coeftest function from the lmtest package in R is used to test the significance of the coefficients in a linear model. Specifically, it provides a summary of the model's coefficients, including their estimates, standard errors, t-values, and p-values. This allows you to assess whether each predictor variable has a statistically significant effect on the dependent variable.
coeftest(did_model, vcov= vcovHC)

#theres heteroscedacity - use robust standard errors (coeftest in appendix)
bptest(did_model, studentize = FALSE)

resettest(did_model, power = 2:3, type = "fitted")

qqnorm(did_model$residuals)
qqline(did_model$residuals,col="red")

plot(y=did_model$residuals, x=did_model$fitted.values, xlab= "Fitted Values", ylab="Residuals")
abline(h=0, col="red")

# Perform the Breusch-Pagan test
bp_test <- bptest(did_model, studentize = FALSE)
print(bp_test)



# Create the Residuals vs Fitted plot
plot(did_model$fitted.values, residuals(did_model),
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2)


library(MASS)

# Fit robust regression model
robust_model <- rlm(log_price ~ close_to_crossrail_network * post_treatment +
                      log_underground_distance  +  log_population_density  + 
                      log_min_distance_to_caz +  semi_detached_binary + detached_binary + 
                      flat_binary + avg_bedrooms + average_qualification_level_4,
                    data = properties_with_census)

# Summarize the robust model
summary(robust_model)

residuals <- residuals(did_model)

# Load necessary package
library(ggplot2)

# Histogram of residuals
ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(binwidth = 0.1, fill = "blue", alpha = 0.7) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency")


library(stargazer)
stargazer(did_model, western_did_model, eastern_did_model, central_did_model, type = "text",
          dep.var.labels = "Price",
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", "Population Density",
                               "CAZ", "Semi-Detached", "Detatched", "Flat",
                               "Bedrooms", "Qualification Level 4", "Close to Crossrail * Post-Treatment"
          ),
          column.labels = c("Full Model", "Western London", "Eastern London", "Central London"),
          out = "did_full.htm")


# model of just detached


# Model for detached properties
detached_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                       log_underground_distance  +  log_population_density  + 
                       log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                     data = subset(properties_with_census, property_type == "D"))

# Summarize the detached model
summary(detached_model)

#western detatched model
# Run the DiD regression model
western_detached_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                               log_underground_distance  +  log_population_density  + 
                               log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                             data = subset(western_subset, property_type == "D"))

# Summarize the model
summary(western_detached_model)

#eastern detatched model
# Run the DiD regression model
eastern_detached_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                               log_underground_distance  +  log_population_density  + 
                               log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                             data = subset(eastern_subset, property_type == "D"))

# Summarize the model
summary(eastern_detached_model)

#central detatched model
# Run the DiD regression model
central_detached_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                               log_underground_distance  +  log_population_density  + 
                               log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                             data = subset(central_subset, property_type == "D"))

# Summarize the model
summary(central_detached_model)

#stargazer
stargazer(detached_model, western_detached_model, eastern_detached_model, type = "text",
          dep.var.labels = "Price",
          #covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", "Population Density",
          #                    "Employment Rate", "Household Deprivation", "CAZ", "Terraced",
          #                     "Flat", "Close to Crossrail * Post-Treatment"
          # ),
          column.labels = c("Full Model", "Western London", "Eastern London", "Central London"),
          out = "detached_results.htm")


stargazer(central_detached_model, type = "text",
          dep.var.labels = "Price",
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", "Population Density",
                               "Employment Rate", "Household Deprivation", "CAZ", "Terraced",
                               "Flat", "Close to Crossrail * Post-Treatment"
          ),
          column.labels = c("Central London"),
          out = "detached_results2.htm")


# model without detached
#full 
# Run the DiD regression model
# model without detached properties
non_detached_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                           log_underground_distance  +  log_population_density  + 
                           log_min_distance_to_caz + semi_detached_binary + flat_binary + avg_bedrooms + average_qualification_level_4, 
                         data = subset(properties_with_census, property_type != "D"))

summary(non_detached_model)

vif(non_detached_model, type = "predictor")

#WESTERN
# Run the DiD regression model
western_non_detached_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                                   log_underground_distance  +  log_population_density  + 
                                   log_min_distance_to_caz + semi_detached_binary + flat_binary + 
                                   avg_bedrooms + average_qualification_level_4, 
                                 data = subset(western_subset, property_type != "D"))

summary(western_non_detached_model)

#EASTERN
# Run the DiD regression model
eastern_non_detached_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                                   log_underground_distance  +  log_population_density  + 
                                   log_min_distance_to_caz + semi_detached_binary + flat_binary + 
                                   avg_bedrooms + average_qualification_level_4, 
                                 data = subset(eastern_subset, property_type != "D"))

summary(eastern_non_detached_model)

#CENTRAL
# Run the DiD regression model
central_non_detached_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                                   log_underground_distance  +  log_population_density  + 
                                   log_min_distance_to_caz + semi_detached_binary + flat_binary + 
                                   avg_bedrooms + average_qualification_level_4, 
                                 data = subset(central_subset, property_type != "D"))

summary(central_non_detached_model)

# Model for detached properties
detached_model <- lm(log_price ~ nearest_crossrail * post_treatment +
                       log_underground_distance  +  log_population_density  + 
                       log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                     data = subset(eastern_subset, property_type == "F"))

# Summarize the detached model
summary(detached_model)

library(lmtest)
library(sandwich)
# could put in appendixes - is it needed? the coeftest function from the lmtest package in R is used to test the significance of the coefficients in a linear model. Specifically, it provides a summary of the model's coefficients, including their estimates, standard errors, t-values, and p-values. This allows you to assess whether each predictor variable has a statistically significant effect on the dependent variable.
coeftest(non_detached_model, vcov= vcovHC)

#theres heteroscedacity - use robust standard errors (coeftest in appendix)
bptest(non_detached_model, studentize = FALSE)
resettest(detached_model, power = 2:3, type = "fitted")

qqnorm(non_detached_model$residuals)
qqline(non_detached_model$residuals,col="red")

plot(y=non_detached_model$residuals, x=non_detached_model$fitted.values, xlab= "Fitted Values", ylab="Residuals")
abline(h=0, col="red")

# Perform the Breusch-Pagan test
bp_test <- bptest(non_detached_model)
print(non_detached_model)

# Create the Residuals vs Fitted plot
plot(non_detached_model$fitted.values, residuals(non_detached_model),
     main = "Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Residuals",
     pch = 20, col = "blue")
abline(h = 0, col = "red", lwd = 2)


library(MASS)

# Fit robust regression model
robust_model <- rlm(log_price ~ close_to_crossrail_network * post_treatment +
                      log_underground_distance  +  log_population_density  + 
                      log_min_distance_to_caz +  semi_detached_binary + detached_binary + 
                      flat_binary + avg_bedrooms + average_qualification_level_4,
                    data = subset(properties_with_census, property_type != "D"))

# Summarize the robust model
summary(robust_model)

residuals <- residuals(non_detached_model)

# Load necessary package
library(ggplot2)

# Histogram of residuals
ggplot(data.frame(residuals), aes(x = residuals)) +
  geom_histogram(binwidth = 0.1, fill = "blue", alpha = 0.7) +
  labs(title = "Histogram of Residuals", x = "Residuals", y = "Frequency")

# Perform the Durbin-Watson test
dw_test <- dwtest(eastern_non_detached_model)
print(dw_test)

library(AER)

subset(properties_with_census, property_type != "D") %>% lm(log_price ~ close_to_crossrail_network * post_treatment +
                                                              log_underground_distance  +  log_population_density  + 
                                                              log_min_distance_to_caz +  semi_detached_binary + detached_binary + 
                                                              flat_binary + avg_bedrooms + average_qualification_level_4, data = . ) %>% plot(which=1) 

#stargazer
stargazer(non_detached_model, western_non_detached_model, eastern_non_detached_model,  type = "text",
          dep.var.labels = "Price",
          #covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", "Population Density",
          #                    "CAZ", "Bedrooms", "Qualification Level 4", "Semi-Detached",
          #                     "Flat", "Close to Crossrail * Post-Treatment"
          # ),
          column.labels = c("Full Model", "Western London", "Eastern London", "Central London"),
          out = "non_detached_results.htm")

stargazer(central_non_detached_model, type = "text",
          dep.var.labels = "Price",
          # covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", "Population Density",
          #                     "Employment Rate", "Household Deprivation", "CAZ", "Terraced",
          #                    "Flat", "Close to Crossrail * Post-Treatment"
          #),
          column.labels = c("Central London"),
          out = "non_detached_results2.htm")


#regular regression
standard_regression <- lm(log_price ~ nearest_crossrail * post_treatment +
                            log_underground_distance  +  log_population_density  + 
                            log_min_distance_to_caz + semi_detached_binary + flat_binary  +
                            avg_bedrooms + average_qualification_level_4,
                          data = subset(properties_with_census, property_type != "D"))

summary(standard_regression)

# Load necessary package

# Check VIF
vif(standard_regression, type = "predictor")

stargazer(non_detached_model, standard_regression, type = "html",
          dep.var.labels = "Price",
          covariate.labels = c("Close to Crossrail", "Nearest Crossrail", "Post-Treatment", "Underground Distance", "Population Density",
                               "CAZ", "Semi-Detached", "Flat", "Bedrooms", "Qualification Level 4",
                               "Close to Crossrail * Post-Treatment", "Nearest Crossrail * Post-Treatment"
          ),
          column.labels = c("DiD Model", "Standard Regression"),
          out = "did_vs_standard_regression.htm")



# Propensity score matching model


# Install and load necessary packages
library(MatchIt)
library(dplyr)
library(cobalt)
library(ggplot2)

# Step 1: Estimate propensity scores and perform matching
# Assuming 'close_to_crossrail_network' is the treatment indicator
matchit_formula <- close_to_crossrail_network ~ post_treatment +
  log_underground_distance  +  log_population_density  + 
  log_min_distance_to_caz + semi_detached_binary + flat_binary + 
  avg_bedrooms + average_qualification_level_4

# remove D from each of the datasets
non_centred_properties_with_census <- subset(non_centred_properties_with_census, property_type != "D")
non_centred_western_subset <- subset(non_centred_western_subset, property_type != "D")
non_centred_eastern_subset <- subset(non_centred_eastern_subset, property_type != "D")
non_centred_central_subset <- subset(non_centred_central_subset, property_type != "D")



# Perform nearest neighbor matching
full_psm <- matchit(matchit_formula, data = non_centred_properties_with_census, method = "nearest")
western_psm <- matchit(matchit_formula, data = non_centred_western_subset, method = "nearest")
eastern_psm <- matchit(matchit_formula, data = non_centred_eastern_subset, method = "nearest")
central_psm <- matchit(matchit_formula, data = non_centred_central_subset, method = "nearest")

set.cobalt.options(binary = "std")

# variable names 
new.names <- c(flat_binary = "Flat",
               terraced_binary = "Terraced",
               close_to_crossrail_network = "Close to Crossrail",
               log_underground_distance = "Underground Distance",
               log_population_density = "Population Density",
               employment_rate = "Employment Rate",
               percent_households_deprived = "Household Deprivation")

# Step 2: Check balance

summary(psm)
full_plot <- love.plot(full_psm,
                       stars = "raw",
                       drop.distance = TRUE, 
                       var.order = "unadjusted",
                       var.names = new.names,
                       abs = TRUE,
                       line = TRUE, 
                       colors = c("#377EB8", "#E41A1C"),
                       thresholds = c(m = .1),
                       position = c(.75, .25))+
  theme(legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

western_plot <- love.plot(western_psm,
                          stars = "raw",
                          drop.distance = TRUE, 
                          var.order = "unadjusted",
                          var.names = new.names,
                          abs = TRUE,
                          line = TRUE, 
                          colors = c("#377EB8", "#E41A1C"),
                          thresholds = c(m = .1),
                          position = c(.75, .25))+
  theme(legend.box.background = element_rect(), 
        legend.box.margin = margin(1, 1, 1, 1))

eastern_plot <- love.plot(eastern_psm,
                          stars = "raw",
                          drop.distance = TRUE, 
                          var.order = "unadjusted",
                          var.names = new.names,
                          abs = TRUE,
                          line = TRUE, 
                          colors = c("#377EB8", "#E41A1C"),
                          thresholds = c(m = .1),
                          position = c(.75, .25))+
  theme(legend.box.background = element_rect(),
        legend.box.margin = margin(1, 1, 1, 1))

central_plot <- love.plot(central_psm,
                          stars = "raw",
                          drop.distance = TRUE, 
                          var.order = "unadjusted",
                          var.names = new.names,
                          abs = TRUE,
                          line = TRUE, 
                          colors = c("#377EB8", "#E41A1C"),
                          thresholds = c(m = .1),
                          position = c(.75, .25))+
  theme(legend.box.background = element_rect(),
        legend.box.margin = margin(1, 1, 1, 1))

#full_plot
#western_plot
#eastern_plot
#central_plot

# Step 3: Extract matched data
full_matched_data <- match.data(full_psm)
western_matched_data <- match.data(western_psm)
eastern_matched_data <- match.data(eastern_psm)
central_matched_data <- match.data(central_psm)

# Step 4: Run the DiD regression model on matched data
full_did_model_matched <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                               log_underground_distance  +  log_population_density  + 
                               log_min_distance_to_caz +  semi_detached_binary  + 
                               flat_binary + avg_bedrooms + average_qualification_level_4,
                             data = full_matched_data)

# Summarize the matched model
summary(full_did_model_matched)

# Run the DiD regression model on matched data
western_did_model_matched <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                                  log_underground_distance  +  log_population_density  + 
                                  log_min_distance_to_caz +  semi_detached_binary  + 
                                  flat_binary + avg_bedrooms + average_qualification_level_4,
                                data = western_matched_data)

# Summarize the matched model
summary(western_did_model_matched)

# Run the DiD regression model on matched data
eastern_did_model_matched <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                                  log_underground_distance  +  log_population_density  + 
                                  log_min_distance_to_caz +  semi_detached_binary  + 
                                  flat_binary + avg_bedrooms + average_qualification_level_4,
                                data = eastern_matched_data)

# Summarize the matched model
summary(eastern_did_model_matched)

# Run the DiD regression model on matched data
central_did_model_matched <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                                  log_underground_distance  +  log_population_density  + 
                                  log_min_distance_to_caz +  semi_detached_binary  + 
                                  flat_binary + avg_bedrooms + average_qualification_level_4,
                                data = central_matched_data)

# Summarize the matched model
summary(central_did_model_matched)

#stargazer
stargazer(full_did_model_matched, western_did_model_matched, eastern_did_model_matched,  type = "text",
          dep.var.labels = "Price",
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", "Population Density",
                               "CAZ", "Semi-Detatched", "Flat",
                               "Bedrooms", "Qualification Level 4", "Close to Crossrail * Post-Treatment"
          ),
          column.labels = c("Full Model", "Western London", "Eastern London", "Central London"),
          out = "psm.htm")

stargazer(central_did_model_matched, type = "text",
          dep.var.labels = "Price",
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", "Population Density",
                               "CAZ", "Semi-Detatched", "Flat",
                               "Bedrooms", "Qualification Level 4", "Close to Crossrail * Post-Treatment"
          ),
          column.labels = c("Central London"),
          out = "psm2.htm")












library(ggplot2)
#samples

# new dataset with just propery type D
detached_properties <- subset(properties_with_census, property_type == "D")
# random sample of 1000 points from properties census
sampled_data <- detached_properties[sample(nrow(non_centred_western_subset), 3000), ]

# sample cr
cr_model_full <- lm(log_price ~ close_to_crossrail_network + post_treatment +
                      log_underground_distance  +  log_population_density  + 
                      log_min_distance_to_caz  + avg_bedrooms + average_qualification_level_4,
                    data = detached_properties)
summary(cr_model_full)
crPlots(cr_model_full)



# summary of the n for variables
#n crossrail, underground stops, 



#####-----------


# Run the DiD regression model
western_detached_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                               log_underground_distance  +  log_population_density  + 
                               log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                             data = subset(western_subset, property_type == "D"))

# Summarize the model
summary(western_detached_model)

#eastern detatched model
# Run the DiD regression model
eastern_detached_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                               log_underground_distance  +  log_population_density  + 
                               log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                             data = subset(eastern_subset, property_type == "D"))

# western flat
western_flat_model <- lm(log_price ~ close_to_crossrail_network + post_treatment +
                           log_underground_distance  +  log_population_density  + 
                           log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                         data = subset(western_subset, property_type == "F"))

eastern_flat_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                           log_underground_distance  +  log_population_density  + 
                           log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                         data = subset(eastern_subset, property_type == "F"))

# western semi-detached
western_semi_detached_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                                    log_underground_distance  +  log_population_density  + 
                                    log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                                  data = subset(western_subset, property_type == "S"))

# eastern semi-detached
eastern_semi_detached_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                                    log_underground_distance  +  log_population_density  + 
                                    log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                                  data = subset(eastern_subset, property_type == "S"))

# western terraced
western_terraced_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                               log_underground_distance  +  log_population_density  + 
                               log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                             data = subset(western_subset, property_type == "T"))

# eastern terraced
eastern_terraced_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                               log_underground_distance  +  log_population_density  + 
                               log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                             data = subset(eastern_subset, property_type == "T"))


# stargazer the detached models
stargazer(western_detached_model, eastern_detached_model,
          dep.var.labels = "Log Price",
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", 
                               "Population Density", "Distance to CAZ", "Avg Bedrooms", 
                               "Qualification Level 4", "Close to Crossrail * Post-Treatment"),
          type = "html", out = "detached_flat_models.htm")

stargazer(western_flat_model, eastern_flat_model,
          type = "html",
          dep.var.labels = "Log Price",
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", 
                               "Population Density", "Distance to CAZ", "Avg Bedrooms", 
                               "Qualification Level 4", "Close to Crossrail * Post-Treatment"),
          out = "flat_models.htm")

stargazer(western_semi_detached_model, eastern_semi_detached_model,
          dep.var.labels = "Log Price",
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", 
                               "Population Density", "Distance to CAZ", "Avg Bedrooms", 
                               "Qualification Level 4", "Close to Crossrail * Post-Treatment"),
          type = "html", out = "semi_detached_models.htm")

stargazer(western_terraced_model, eastern_terraced_model,
          dep.var.labels = "Log Price",
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", 
                               "Population Density", "Distance to CAZ", "Avg Bedrooms", 
                               "Qualification Level 4", "Close to Crossrail * Post-Treatment"),
          type = "html", out = "terraced_models.htm")





# central and whole model 
central_detached_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                               log_underground_distance  +  log_population_density  + 
                               log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                             data = subset(central_subset, property_type == "D"))

central_flat_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                           log_underground_distance  +  log_population_density  + 
                           log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                         data = subset(central_subset, property_type == "F"))

central_semi_detached_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                                    log_underground_distance  +  log_population_density  + 
                                    log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                                  data = subset(central_subset, property_type == "S"))

central_terraced_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                               log_underground_distance  +  log_population_density  + 
                               log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                             data = subset(central_subset, property_type == "T"))


whole_detached_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                             log_underground_distance  +  log_population_density  + 
                             log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                           data = subset(properties_with_census, property_type == "D"))

whole_flat_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                         log_underground_distance  +  log_population_density  + 
                         log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                       data = subset(properties_with_census, property_type == "F"))

whole_semi_detached_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                                  log_underground_distance  +  log_population_density  + 
                                  log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                                data = subset(properties_with_census, property_type == "S"))

whole_terraced_model <- lm(log_price ~ close_to_crossrail_network * post_treatment +
                             log_underground_distance  +  log_population_density  + 
                             log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                           data = subset(properties_with_census, property_type == "T"))

stargazer(central_detached_model, central_flat_model, 
          dep.var.labels = "Log Price",
          column.labels = c("Detached", "Flat"),
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", 
                               "Population Density", "Distance to CAZ", "Avg Bedrooms", 
                               "Qualification Level 4", "Close to Crossrail * Post-Treatment"),
          type = "html", out = "central_models1.htm")

stargazer(central_semi_detached_model, central_terraced_model,
          dep.var.labels = "Log Price",
          column.labels = c("Semi", "Terraced"),
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", 
                               "Population Density", "Distance to CAZ", "Avg Bedrooms", 
                               "Qualification Level 4", "Close to Crossrail * Post-Treatment"),
          type = "html", out = "central_models2.htm")

stargazer(whole_detached_model, whole_flat_model, 
          dep.var.labels = "Log Price",
          column.labels = c("Detached", "Flat"),
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", 
                               "Population Density", "Distance to CAZ", "Avg Bedrooms", 
                               "Qualification Level 4", "Close to Crossrail * Post-Treatment"),
          type = "html", out = "whole_models.htm")

stargazer(whole_semi_detached_model, whole_terraced_model,
          dep.var.labels = "Log Price",
          column.labels = c("Semi", "Terraced"),
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", 
                               "Population Density", "Distance to CAZ", "Avg Bedrooms", 
                               "Qualification Level 4", "Close to Crossrail * Post-Treatment"),
          type = "html", out = "whole_models2.htm")




resettest(western_detached_model, power = 2:3, type = "fitted")
resettest(eastern_detached_model, power = 2:3, type = "fitted")
resettest(western_flat_model, power = 2:3, type = "fitted")
resettest(eastern_flat_model, power = 2:3, type = "fitted")
resettest(western_semi_detached_model, power = 2:3, type = "fitted")
resettest(eastern_semi_detached_model, power = 2:3, type = "fitted")
resettest(western_terraced_model, power = 2:3, type = "fitted")
resettest(eastern_terraced_model, power = 2:3, type = "fitted")

resettest(central_detached_model, power = 2:3, type = "fitted")
resettest(central_flat_model, power = 2:3, type = "fitted")
resettest(central_semi_detached_model, power = 2:3, type = "fitted")
resettest(central_terraced_model, power = 2:3, type = "fitted")
resettest(whole_detached_model, power = 2:3, type = "fitted")
resettest(whole_flat_model, power = 2:3, type = "fitted")
resettest(whole_semi_detached_model, power = 2:3, type = "fitted")
resettest(whole_terraced_model, power = 2:3, type = "fitted")

# Load necessary package
install.packages("stargazer")
library(stargazer)

# Create a data frame with the RESET test results
reset_test_results <- data.frame(
  Model = c("Western Detached", "Eastern Detached", "Central Detached", "Whole Detached",
            "Western Flat", "Eastern Flat", "Central Flat", "Whole Flat",
            "Western Semi-Detached", "Eastern Semi-Detached", "Central Semi-Detached", "Whole Semi-Detached",
            "Western Terraced", "Eastern Terraced", "Central Terraced", "Whole Terraced"),
  RESET = c(28.6, 51.618, 6.0027, 53.696, 
            209.75, 440.99, 357.46, 2860.4, 
            137.61, 89.877, 29.44, 627.79, 
            445.75, 358.55, 564.68, 3011.6),
  df1 = c(2, 2, 2, 2, 
          2, 2, 2, 2, 
          2, 2, 2, 2, 
          2, 2, 2, 2),
  df2 = c(7757, 5070, 202, 17919, 
          39941, 48867, 83240, 246917, 
          19176, 18620, 626, 46302, 
          31921, 53142, 10639, 106029),
  p_value = c("4.215e-13", "< 2.2e-16", "0.002935", "< 2.2e-16", 
              "< 2.2e-16", "< 2.2e-16", "< 2.2e-16", "< 2.2e-16", 
              "< 2.2e-16", "< 2.2e-16", "6.034e-13", "< 2.2e-16", 
              "< 2.2e-16", "< 2.2e-16", "< 2.2e-16", "< 2.2e-16")
)

# Print the table using stargazer
stargazer(reset_test_results, type = "html", summary = FALSE, rownames = FALSE,
          title = "Comparison of RESET Tests for Different Models",
          column.labels = c("Model", "RESET", "df1", "df2", "p-value"),
          out="full_reset_test_results.htm")

# Create Q-Q plots for the four models in a 2x2 layout
par(mfrow = c(2, 2))  # Set up a 2x2 plotting layout

# Q-Q plot for western_detached_model
qqnorm(western_detached_model$residuals, main = "Q-Q Plot for Western Detached")
qqline(western_detached_model$residuals, col = "red")

# Q-Q plot for eastern_detached_model
qqnorm(western_flat_model$residuals, main = "Q-Q Plot for Western Flat")
qqline(western_flat_model$residuals, col = "red")

# Q-Q plot for western_semi_detached_model
qqnorm(western_semi_detached_model$residuals, main = "Q-Q Plot for Western Semi-Detached")
qqline(western_semi_detached_model$residuals, col = "red")

# Q-Q plot for western_terraced_model
qqnorm(western_terraced_model$residuals, main = "Q-Q Plot for Western Terraced")
qqline(western_terraced_model$residuals, col = "red")

# Reset to default layout
par(mfrow = c(1, 1))



crPlots(western_flat_model)






