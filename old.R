

#eastern bbox london model
library(sf)

# Define the bounding box coordinates
west_lon <- -0.029182
south_lat <- 51.518429
east_lon <- 0.423523
north_lat <- 51.683148

# Create a polygon representing the bounding box
bbox <- st_polygon(list(rbind(
  c(west_lon, south_lat),
  c(east_lon, south_lat),
  c(east_lon, north_lat),
  c(west_lon, north_lat),
  c(west_lon, south_lat)
)))

library(sf)

# Convert properties_with_census to an sf object if it's not already
if (!inherits(properties_with_census, "sf")) {
  properties_with_census <- st_as_sf(properties_with_census)
}


# Set the CRS for properties_with_census to EPSG:4326
properties_with_census <- st_transform(properties_with_census, crs = 4326)

# Set the CRS for the bounding box to EPSG:4326
bbox_sf <- st_transform(bbox_sf, crs = 4326)


# Convert to an sf object
bbox_sf <- st_sfc(bbox, crs = st_crs(properties_with_census))

# Ensure the properties data is in the correct sf format
properties_east_sf <- st_as_sf(properties_with_census)

# Filter properties within the bounding box
properties_east <- st_intersection(properties_east_sf, bbox_sf)

# Run the DiD regression model on the filtered dataset
did_model_east <- lm(price ~ close_to_crossrail * post_treatment + 
                       log_underground_distance + population_density + 
                       employment_rate + percent_households_deprived + 
                       detached_binary + terraced_binary + flat_binary,
                     data = properties_east)

# Summarize the model
summary(did_model_east)

# west bbox model 

west_west_lon <- -1.110168
west_south_lat <- 51.363917
west_east_lon <- -0.223022
west_north_lat <- 51.565937

# Create a polygon representing the bounding box
west_bbox <- st_polygon(list(rbind(
  c(west_west_lon, west_south_lat),
  c(west_east_lon, west_south_lat),
  c(west_east_lon, west_north_lat),
  c(west_west_lon, west_north_lat),
  c(west_west_lon, west_south_lat)
)))

# Convert the bounding box to an sf object
west_bbox_sf <- st_sfc(west_bbox, crs = 4326)

# Convert properties_with_census to an sf object if it's not already
if (!inherits(properties_with_census, "sf")) {
  properties_with_census <- st_as_sf(properties_with_census)
}


# Set the CRS for properties_with_census to EPSG:4326
properties_with_census <- st_transform(properties_with_census, crs = 4326)

# Set the CRS for the bounding box to EPSG:4326
west_bbox_sf <- st_transform(west_bbox_sf, crs = 4326)


# Convert to an sf object
west_bbox_sf <- st_sfc(west_bbox, crs = st_crs(properties_with_census))

# Ensure the properties data is in the correct sf format
properties_west_sf <- st_as_sf(properties_with_census)

# Filter properties within the bounding box
properties_west <- st_intersection(properties_west_sf, west_bbox_sf)

# Run the DiD regression model on the filtered dataset
did_model_west <- lm(price ~ close_to_crossrail * post_treatment + 
                       log_underground_distance + population_density + 
                       employment_rate + percent_households_deprived + 
                       detached_binary + terraced_binary + flat_binary,
                     data = properties_west)

# Summarize the model
summary(did_model_west)





#spatial test
# Ensure the properties_with_census dataset is an sf object
sampled_data <- st_as_sf(properties_with_census)

# Extract coordinates
coordinates <- st_coordinates(properties_with_census)

# Add longitude and latitude to the dataframe
sampled_data$longitude <- coordinates[, 1]
sampled_data$latitude <- coordinates[, 2]

library(spdep)
library(sp)
sampled_data <- st_as_sf(sampled_data, coords = c("longitude", "latitude"), crs = 4326)

coordinates(sampled_data) <- ~longitude + latitude
proj4string(sampled_data) <- CRS("+init=epsg:4326")  # Specify appropriate CRS

library(spdep)

# Create a neighbors list using k-nearest neighbors
coords <- st_coordinates(sampled_data)
nb <- knn2nb(knearneigh(coords, k = 8))

# Convert neighbors list to a listw object
lw <- nb2listw(nb, style = "W")

# Run the initial linear model
did_model_log <- lm(log_price ~ close_to_crossrail * post_treatment +
                      log_underground_distance + log_population_density + 
                      employment_rate + percent_households_deprived + 
                      terraced_binary + flat_binary,
                    data = sampled_data)

# Calculate Moran's I on the residuals
residuals <- residuals(did_model_log)
moran_test <- moran.test(residuals, lw)

print(moran_test)

library(spatialreg)

# Fit a Spatial Lag Model
slm_model <- lagsarlm(log_price ~ close_to_crossrail * post_treatment +
                        log_underground_distance + log_population_density + 
                        employment_rate + percent_households_deprived + 
                        terraced_binary + flat_binary,
                      data = sampled_data, listw = lw)

# Summarize the Spatial Lag Model
summary(slm_model)


#using sample
# Install and load necessary packages
library(MatchIt)
library(dplyr)
library(cobalt)


# Step 1: Estimate propensity scores and perform matching
# Assuming 'close_to_crossrail' is the treatment indicator
matchit_formula <- close_to_crossrail ~ log_underground_distance + log_population_density + 
  employment_rate + percent_households_deprived + log_min_distance_to_caz +
  terraced_binary + flat_binary

# Perform nearest neighbor matching
psm <- matchit(matchit_formula, data = sampled_data, method = "nearest")
set.cobalt.options(binary = "std")

# variable names 
new.names <- c(flat_binary = "Flat",
               terraced_binary = "Terraced",
               close_to_crossrail = "Close to Crossrail",
               log_underground_distance = "Underground Distance",
               log_population_density = "Population Density",
               employment_rate = "Employment Rate",
               percent_households_deprived = "Household Deprivation")

# Step 2: Check balance
library(ggplot2)

summary(psm)
sample_plot <- love.plot(psm,
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

sample_plot
#need to define stars in text https://cran.r-project.org/web/packages/cobalt/vignettes/love.plot.html

# Step 3: Extract matched data
matched_data <- match.data(psm)

# Step 4: Run the DiD regression model on matched data
did_model_matched <- lm(log_price ~ close_to_crossrail * post_treatment +
                          log_underground_distance  +  log_population_density  + 
                          employment_rate + percent_households_deprived + 
                          terraced_binary + flat_binary,
                        data = matched_data)

# Summarize the matched model
summary(did_model_matched)


#regular west
west_regression <- lm(log_price ~ nearest_crossrail * post_treatment +
                        log_underground_distance  +  log_population_density  + 
                        employment_rate + percent_households_deprived + log_min_distance_to_caz +
                        terraced_binary + flat_binary,
                      data = western_subset)

summary(west_regression)

# Check VIF
vif(west_regression, type = "predictor")

#regular east
east_regression <- lm(log_price ~ nearest_crossrail * post_treatment +
                        log_underground_distance  +  log_population_density  + 
                        employment_rate + percent_households_deprived + log_min_distance_to_caz +
                        terraced_binary + flat_binary,
                      data = eastern_subset)

summary(east_regression)

# Check VIF
vif(east_regression, type = "predictor")

#regular centre
central_regression <- lm(log_price ~ nearest_crossrail * post_treatment +
                           log_underground_distance  +  log_population_density  + 
                           employment_rate + percent_households_deprived + log_min_distance_to_caz +
                           terraced_binary + flat_binary,
                         data = central_subset)

summary(central_regression)

# Check VIF
vif(central_regression, type = "predictor")