

# test data first 5 rows
#test_data <- head(data)
#
#create_address <- function(paon, saon, street, locality, city, district, county, postcode) {
#  address_parts <- c(paon, saon, street, locality, city, district, county, postcode)
#  address_parts <- address_parts[address_parts != ""]
#  address <- paste(address_parts, collapse=", ")
#  return(address)
#}

# Create the 'address' column by concatenating the specified columns
#test_data$address <- mapply(create_address, test_data$paon, test_data$saon, test_data$street, test_data$locality, test_data$city, 
#                           test_data$district, test_data$county, test_data$postcode)

# Print the first few rows to verify the 'address' column
#head(test_data$address)




#Geocode
#library(tidyverse)
#library(tidygeocoder)
#library(sf)
#library(mapview)
#library(dplyr)

#test_data <- test_data %>%
#  tidygeocoder::geocode(
#   address = address,
#  method = "osm"
#)
