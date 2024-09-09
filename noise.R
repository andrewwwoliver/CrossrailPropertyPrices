library(sf)
library(dplyr)

# Read the noise shapefile
noise_england_sf <- st_read("data/Noise/Road_Noise_LAeq16h_England_Round_3.shp")

# Transform the CRS of noise_england_sf to match that of filtered_properties_sf
noise_england_sf <- st_transform(noise_england_sf, crs = st_crs(detached_subset))

# Define the bounding box coordinates and crop both datasets
bbox <- st_bbox(c(xmin = -1.02, xmax = 0.4, ymin = 51.4, ymax = 51.7), crs = st_crs(detached_subset))
noise_england_sf <- st_crop(noise_england_sf, bbox)

# Validate and correct invalid geometries in noise_england_sf
invalid_geometries <- st_is_valid(noise_england_sf)
if (any(!invalid_geometries)) {
  cat("Invalid geometries detected. Correcting them...\n")
  noise_england_sf <- st_make_valid(noise_england_sf)
}

##### all
# load 
load("properties_with_census_df.RData")

#drop geometry.y and rename geometry.x to geometry
all_noise <- properties_with_census_df %>% select(-geometry.y) %>% rename(geometry = geometry.x)

#set as sf
all_noise <- st_as_sf(all_noise)

#check the geometry of filtered
st_crs(all_noise)

# Perform the spatial join
all_noise_properties_sf <- st_join(all_noise, noise_england_sf, join = st_within)

# Check the first few rows of the result
head(all_noise_properties_sf)

noise_properties_sf <- all_noise_properties_sf %>%
  # select id.x and noiseclass
  select(id.x, noiseclass)


#levels in the noiseclass
levels(noise_properties_sf$noiseclass)

#replace NA with <55
noise_properties_sf$noiseclass[is.na(noise_properties_sf$noiseclass)] <- "<55.0"

# make levels a factor
noise_properties_sf$noiseclass <- as.factor(noise_properties_sf$noiseclass)

#make df
noise_properties_sf <- as.data.frame(noise_properties_sf)

#drop geometry
noise_properties_sf <- noise_properties_sf %>% select(-geometry)

#relevel noiseclass to merge 55.0-59.9 , 60.0-64.99 and 65.0-69.9 and 70.0-74.9 and >=75.0
library(dplyr)
library(forcats)


# Relevel noiseclass into <55 and >=55.0
noise_properties_sf <- noise_properties_sf %>%
  mutate(noiseclass = as.character(noiseclass),  # Convert to character if not already
         noiseclass = fct_collapse(noiseclass,
                                   `<55.0` = c("<55.0"),
                                   `>=55.0` = c("55.0-59.9", "60.0-64.9", "65.0-69.9", "70.0-74.9", ">=75.0"))) %>%
  mutate(noiseclass = fct_relevel(noiseclass, "<55.0", ">=55.0"))  # Reorder if needed

# View the updated noiseclass levels
levels(noise_properties_sf$noiseclass)

#table for each
table(noise_properties_sf$noiseclass)

# save as rdata
save(noise_properties_sf, file = "noise_properties_sf.RData")
