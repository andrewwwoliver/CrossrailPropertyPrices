
# Load necessary libraries
library(dplyr)
library(sf)
library(readr)
# Read CSV files into data frames
education_data <- read_csv("education.csv")
bedrooms_data <- read_csv("bedrooms.csv")
occupation_data <- read_csv("occupation.csv")
unemployment_data <- read_csv("unemployment.csv")
deprivation_data <- read_csv("deprivation.csv")
pop_density_data <- read_csv("pop_density.csv")

# Read GeoJSON file into a spatial data frame
output_areas <- st_read("Output_Areas.geojson")

#change crs of output areas to 4326
output_areas <- st_transform(output_areas, crs = 4326)

# Ensure the geography code columns are of the same type (character)
education_data$geography <- as.character(education_data$geography)
bedrooms_data$geography <- as.character(bedrooms_data$geography)
occupation_data$geography <- as.character(occupation_data$geography)
unemployment_data$geography <- as.character(unemployment_data$geography)
deprivation_data$geography <- as.character(deprivation_data$geography)
pop_density_data$geography <- as.character(pop_density_data$geography)
output_areas$OA21CD <- as.character(output_areas$OA21CD)

# Merge all CSV data by 'geography' column
census_data <- education_data %>%
  inner_join(bedrooms_data, by = "geography") %>%
  inner_join(occupation_data, by = "geography") %>%
  inner_join(unemployment_data, by = "geography") %>%
  inner_join(deprivation_data, by = "geography") %>%
  inner_join(pop_density_data, by = "geography")

# Filter combined data to keep only those areas present in the GeoJSON file
filtered_census_data <- census_data %>%
  filter(geography %in% output_areas$OA21CD)

# Join filtered census data with the GeoJSON data to provide geographical context
census_data <- output_areas %>%
  inner_join(filtered_census_data, by = c("OA21CD" = "geography"))

# Subtract the values in column 20 from the values in column 13
census_data <- census_data %>%
  mutate(`Highest level of qualification: Total: All usual residents aged 16 years and over` = 
           `Highest level of qualification: Total: All usual residents aged 16 years and over` - 
           `Highest level of qualification: Other qualifications`)

# Remove column 20 from the data
census_data <- census_data %>%
  select(-`Highest level of qualification: Other qualifications`)

# Calculate the proportion of people with level 4 or above qualifications
census_data <- census_data %>%
  mutate(average_qualification_level_4 = 
           `Highest level of qualification: Level 4 qualifications and above` / 
           `Highest level of qualification: Total: All usual residents aged 16 years and over`)

# Remove the specified columns
columns_to_remove <- c("Highest level of qualification: Total: All usual residents aged 16 years and over",
                       "Highest level of qualification: No qualifications",
                       "Highest level of qualification: Level 1 and entry level qualifications",
                       "Highest level of qualification: Level 2 qualifications",
                       "Highest level of qualification: Apprenticeship",
                       "Highest level of qualification: Level 3 qualifications",
                       "Highest level of qualification: Level 4 qualifications and above")

census_data <- census_data %>%
  select(-all_of(columns_to_remove))

# List of columns to remove
columns_to_remove <- c("FID",
                       "LSOA21CD",
                       "LSOA21NM",
                       "LSOA21NMW",
                       "BNG_E",
                       "BNG_N",
                       "LAT",
                       "LONG",
                       "GlobalID",
                       "date.x",
                       "geography code.x",
                       "date.y",
                       "geography code.y",
                       "date.x.x",
                       "geography code.x.x",
                       "date.y.y",
                       "geography code.y.y",
                       "date.x.x.x",
                       "geography code.x.x.x",
                       "date.y.y.y",
                       "geography code.y.y.y")

# Remove the specified columns
census_data <- census_data %>%
  select(-all_of(columns_to_remove))

# Calculate average number of bedrooms per household
census_data <- census_data %>%
  mutate(
    avg_bedrooms = (`Number of bedrooms: 1 bedroom` * 1 +
                      `Number of bedrooms: 2 bedrooms` * 2 +
                      `Number of bedrooms: 3 bedrooms` * 3 +
                      `Number of bedrooms: 4 or more bedrooms` * 4) / 
      `Number of bedrooms: Total: All households`
  )

# List of bedroom columns to remove
bedroom_columns_to_remove <- c("Number of bedrooms: Total: All households",
                               "Number of bedrooms: 1 bedroom",
                               "Number of bedrooms: 2 bedrooms",
                               "Number of bedrooms: 3 bedrooms",
                               "Number of bedrooms: 4 or more bedrooms")

# Remove the specified bedroom columns
census_data <- census_data %>%
  select(-all_of(bedroom_columns_to_remove))

# Calculate the unemployment rate
census_data <- census_data %>%
  mutate(
    employment_rate = `Occupation (current): Total: All usual residents aged 16 years and over in employment the week before the census` / 
      (`Occupation (current): Total: All usual residents aged 16 years and over in employment the week before the census` + 
         `Unemployment history: Total: All usual residents aged 16 years and over not in employment the week before the census`)
  )

# List of columns to remove
occupation_and_unemployment_columns_to_remove <- c(
  "Occupation (current): Total: All usual residents aged 16 years and over in employment the week before the census",
  "Occupation (current): 1. Managers, directors and senior officials",
  "Occupation (current): 2. Professional occupations",
  "Occupation (current): 3. Associate professional and technical occupations",
  "Occupation (current): 4. Administrative and secretarial occupations",
  "Occupation (current): 5. Skilled trades occupations",
  "Occupation (current): 6. Caring, leisure and other service occupations",
  "Occupation (current): 7. Sales and customer service occupations",
  "Occupation (current): 8. Process, plant and machine operatives",
  "Occupation (current): 9. Elementary occupations",
  "Unemployment history: Total: All usual residents aged 16 years and over not in employment the week before the census",
  "Unemployment history: Not in employment: Worked in the last 12 months",
  "Unemployment history: Not in employment: Not worked in the last 12 months",
  "Unemployment history: Not in employment: Never worked"
)

# Remove the specified columns
census_data <- census_data %>%
  select(-all_of(occupation_and_unemployment_columns_to_remove))

# Calculate the total number of deprived households
census_data <- census_data %>%
  mutate(
    total_deprived_households = `Household deprivation: Household is deprived in one dimension; measures: Value` + 
      `Household deprivation: Household is deprived in two dimensions; measures: Value` + 
      `Household deprivation: Household is deprived in three dimensions; measures: Value` + 
      `Household deprivation: Household is deprived in four dimensions; measures: Value`,
    percent_households_deprived = (total_deprived_households / `Household deprivation: Total: All households; measures: Value`)
  )

# List of columns to remove
deprivation_columns_to_remove <- c(
  "Household deprivation: Total: All households; measures: Value",
  "Household deprivation: Household is not deprived in any dimension; measures: Value",
  "Household deprivation: Household is deprived in one dimension; measures: Value",
  "Household deprivation: Household is deprived in two dimensions; measures: Value",
  "Household deprivation: Household is deprived in three dimensions; measures: Value",
  "Household deprivation: Household is deprived in four dimensions; measures: Value",
  "total_deprived_households"
)

# Remove the specified columns
census_data <- census_data %>%
  select(-all_of(deprivation_columns_to_remove))

# Print the first few rows to verify the changes
head(census_data)

# Rename the population density column
census_data <- census_data %>%
  rename(population_density = `Population Density: Persons per square kilometre; measures: Value`)

#save census data
save(census_data, file = "census_data.RData")

