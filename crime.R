library(dplyr)
library(readr)

# Define the base directory where the data is stored
base_dir <- "data/CrimeJune22May23"

# List all CSV files within the monthly folders
csv_files <- list.files(base_dir, pattern = "*.csv", full.names = TRUE, recursive = TRUE)

# Read and combine all CSV files into one data frame
crime_data <- csv_files %>%
  lapply(read_csv) %>%  # Read each CSV file
  bind_rows()  # Combine all data frames into one

# Check the first few rows of the combined dataset
head(crime_data)

# Assuming 'crime_data' is your combined dataset

# Group by 'LSOA code' and count the number of crimes
crime_count_by_lsoa <- crime_data %>%
  group_by(`LSOA code`) %>%
  summarise(crime_count = n())

# View the first few rows of the result
head(crime_count_by_lsoa)

#rename LSOA code to LSOA11CD
crime_count_by_lsoa <- crime_count_by_lsoa %>%
  rename(LSOA11CD = `LSOA code`)

#add log of crime count (+1 to avoid 0)
crime_count_by_lsoa <- crime_count_by_lsoa %>%
  mutate(log_crime_count = log(crime_count + 1))

#save crime_count_by_lsoa as rdata
save("crime_count_by_lsoa", file = "crime_count_by_lsoa.rdata")



