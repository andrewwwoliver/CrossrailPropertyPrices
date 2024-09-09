###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 1:                             ###
###                          TRANSFORM TO GTFS                          ###
###                                                                     ###
###########################################################################
###########################################################################


# Load packages -----------------------------------------------------------


remotes::install_github("ITSleeds/UK2GTFS")
library(UK2GTFS)
library(tidyverse)
library(sf)
library(mapview)


# ATOC files to GTFS - (rail) ---------------------------------------------

# Transform atoc to gtfs
path_in <- "ttis181.zip"
ttis181 <- atoc2gtfs(path_in = path_in, shapes = TRUE, ncores = 14)

## Inspect output
# Calendar
glimpse(ttis181$calendar)
summary(parse_date(ttis181$calendar$start_date, format = "%Y%m%d"))
summary(parse_date(ttis181$calendar$end_date, format = "%Y%m%d"))
barplot(table(ttis181$calendar$end_date))

# Stops
ttis181$stops %>%
  st_as_sf(coords = c('stop_lon', 'stop_lat'), crs = 4326) %>%
  mapview()

# Check internal validity
UK2GTFS::gtfs_validate_internal(ttis181)



## Force valid. This function does not fix problems it just removes them
ttis181_gtfs <- UK2GTFS::gtfs_force_valid(ttis181)
## Compare original and valid
# Find difference
map2(ttis181, ttis181_gtfs, identical)
# Stop times not included in GTFS version
anti_join(ttis181$stop_times, ttis181_gtfs$stop_times)


## Write as GTFS
UK2GTFS::gtfs_write(ttis181_gtfs, folder = 'atoc/', name = 'ttis181.gtfs')

# Clean env.
rm(list = ls())
gc(reset = TRUE)

# Check GTFS ------------------------------------------------------------

library(tidytransit)
library(lubridate)


library(gtfstools)


# Read GTFS data for trains
gtfs_path <- 'atoc/ttis181.gtfs.zip'
train_gtfs <- read_gtfs(gtfs_path, quiet = FALSE)
summary(train_gtfs)

# Define bounding box coordinates
xmin <- -2.3916
ymin <- 50.6564
xmax <- 1.5772
ymax <- 52.5753

# Create bounding box as an sf object
bbox <- st_as_sfc(st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = st_crs(4326)))

# Filter stops within the bounding box
stops_train <- gtfstools::convert_stops_to_sf(train_gtfs)
stops_train <- stops_train[st_intersects(stops_train, bbox, sparse = FALSE), ]

# Filter GTFS data based on filtered stops
train_se <- filter_by_stop_id(train_gtfs, unique(stops_train$stop_id))

# Write the filtered GTFS data to a new file
write_gtfs(train_se, 'data/southeast_train.gtfs.zip')


# Clean
rm(train_gtfs)
gc()

# Transxchange files to GTFS - (bus) --------------------------------
# Load necessary libraries
library(gtfstools)
library(sf)
library(tidyverse)

# Read transxchange GTFS
england_bus_gtfs <- read_gtfs('itm_england_gtfs.zip', quiet = FALSE)
summary(england_bus_gtfs)

# Define bounding box coordinates
xmin <- -2.3916
ymin <- 50.6564
xmax <- 1.5772
ymax <- 52.5753

# Create bounding box as an sf object
bbox <- st_as_sfc(st_bbox(c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax), crs = st_crs(4326)))

# Filter stops within the bounding box
stops_se <- gtfstools::convert_stops_to_sf(england_bus_gtfs)
stops_se <- stops_se[st_intersects(stops_se, bbox, sparse = FALSE), ]

# Filter GTFS data based on filtered stops
bus_se <- filter_by_stop_id(england_bus_gtfs, unique(stops_se$stop_id))

# Write the filtered GTFS data to a new file
write_gtfs(bus_se, 'data/southeast_bus.gtfs.zip')



# Clean env.
rm(list = ls())
gc(reset = TRUE)


############################################################################
############################################################################
###                                                                      ###
###                      TRANSFORM ATOC (RAIL) DATA                      ###
###                                                                      ###
############################################################################
############################################################################

# Date: December 2022

# THIS CODE IS FOR REFERECE ONLY
# YOU DO NOT NEED TO RUN THIS SCRIPT AS DATA ALREADY EXIST IN REPOSITORY


# Packages ----------------------------------------------------------------

# Uses UK2GTFS V. ‘0.1.1’
# More info: https://itsleeds.github.io/UK2GTFS/articles/ATOC.html
# remotes::install_github("ITSleeds/UK2GTFS")
library(UK2GTFS)
library(tidyverse)
library(gtfstools)
library(sf)


# Transform ATOC (rail data) ----------------------------------------------

# Requires manual download
# Source: https://data.atoc.org/?q=user

# Detect number of cores
n_cores <- parallel::detectCores() -1

# Transform ATOC to GTFS
path_in <- "data/ttis181.zip"
ttis181 <- atoc2gtfs(path_in = path_in, shapes = TRUE, ncores = n_cores)

# Save as GTFS ------------------------------------------------------------

# Check internal validity
UK2GTFS::gtfs_validate_internal(ttis181)

## Force valid. This function does not fix problems, it just removes them
ttis181_gtfs <- UK2GTFS::gtfs_force_valid(ttis181)

## Compare original and valid
# Find difference
map2(ttis181, ttis181_gtfs, identical)
# Stops not included in GTFS version
gtfs_diff <- anti_join(ttis181$stop_times, ttis181_gtfs$stop_times)
gtfs_diff
# Stops missing
unique(gtfs_diff$stop_id)
# Frequency
count(gtfs_diff, stop_id)
# Trips affected
unique(gtfs_diff$trip_id)

## Write as GTFS
UK2GTFS::gtfs_write(ttis181_gtfs, folder = 'data/', name = 'ttis181.gtfs')

# Clean env.
rm(list = ls())
gc(reset = TRUE)


# Filter relevant train services ------------------------------------------

# Full GTFS
ttis181_gtfs <- read_gtfs('data/ttis181.gtfs.zip')

# Define london area
london_boundary <- data.frame(x = -0.118092, y = 51.509865) %>% 
  st_as_sf(coords = c('x', 'y'), crs = 4326) %>% 
  st_buffer(150e3)

# Filter stops in london
stops_london <- gtfstools::convert_stops_to_sf(ttis181_gtfs) 
stops_london <- stops_london[london_boundary,]

# Filter services connected to london
ttis_london <- filter_by_stop_id(ttis181_gtfs, unique(stops_london$stop_id))

# Write london train services
write_gtfs(ttis_london, 'data/london_rail.gtfs.zip')

# Remove full GTFS rail file
unlink('data/ttis181.gtfs.zip')

# osmextract version ‘0.4.1’
library(osmextract)
library(mapview)

# Road and pedestrian network ---------------------------------------------

# PBF road network for Glasgow
england_url <- oe_match('England')
# Download  PBF
oe_download(file_url = england_url$url, download_directory = "./data/")

# Visualize
england_pbf <- osmextract::oe_read('data/geofabrik_england-latest.osm.pbf')
mapview(england_pbf)


# Bus time table data -----------------------------------------------------

# Bus open data (BOD) for Scotland (GTFS format)
url <- "https://data.bus-data.dft.gov.uk/timetable/download/gtfs-file/england/"
download.file(url = url, destfile = "data/20240808_itm_england_gtfs.zip")

# Clean env.
rm(list = ls())
gc(reset = TRUE)


##### TRANSX TO GTFS
library(UK2GTFS)
path_in <- "TransX.zip"
gtfs <- transxchange2gtfs(path_in = path_in,
                          ncores = 14)

gtfs2 <- gtfs_merge(gtfs, force = TRUE, quiet = TRUE)

gtfs_write(gtfs2, folder = getwd(), name = "gtfs")
