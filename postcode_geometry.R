# read in postcode shapefile
postcode_sf <- st_read("data/Postcodes.shp")

library(sf)
library(dplyr)
library(purrr)
library(fs)

# Define the directory containing the .zip files
zip_dir <- "data/Postcodes"

# List all .zip files in the directory
zip_files <- dir_ls(zip_dir, regexp = "\\.zip$")

# Create a temporary directory to unzip the files
temp_dir <- tempdir()

# Function to unzip, read, and return an sf object
read_shapefile_from_zip <- function(zip_file) {
  # Unzip the file into a subdirectory within the temp_dir to avoid overwriting
  unzip_dir <- file.path(temp_dir, tools::file_path_sans_ext(basename(zip_file)))
  unzip(zip_file, exdir = unzip_dir)
  
  # Find the shapefile in the unzipped folder
  shapefile <- dir_ls(unzip_dir, regexp = "\\.shp$", recurse = TRUE)
  
  # Ensure only one shapefile is read
  if(length(shapefile) != 1) {
    stop("Expected exactly one shapefile, but found ", length(shapefile))
  }
  
  # Read the shapefile using sf
  st_read(shapefile, quiet = TRUE)
}

# Apply the function to all zip files and capture results
combined_sf_results <- map(zip_files, safely(read_shapefile_from_zip))

# Extract only the successful results
successful_results <- map(combined_sf_results, "result")

# Filter out any NULL results that might have occurred due to errors
valid_results <- compact(successful_results)

# Combine all valid sf objects into one
combined_sf <- bind_rows(valid_results)

# Check if combined_sf is empty
if (nrow(combined_sf) == 0) {
  stop("The combined sf object is empty. Please check the shapefiles for issues.")
} else {
  # Save the combined shapefile
  st_write(combined_sf, "data/combined_shapefile.shp")
}

# Optionally clean up the temporary directory
unlink(temp_dir, recursive = TRUE)

# Print the combined shapefile
print(combined_sf)

#filter combined sf to only include postcodes within properties_df (PROPERTIES_DF)
postcode_sf <- combined_sf %>%
  filter(POSTCODE %in% properties_sf$postcode)

#save  postcode_sf as rdata
save(postcode_sf, file = "Postcode_sf.RData")