# Load necessary library
library(stargazer)

# Define the stations for each dataset
west_crossrail_stations <- c("Reading Rail Station", "Twyford Rail Station", "Taplow Rail Station", "Burnham (Berks) Rail Station", 
                             "Slough Rail Station", "Langley (Berks) Rail Station", "Iver Rail Station", "West Drayton Station",
                             "Heathrow Terminal 4", "Heathrow Terminal 5", "Heathrow Terminals 2 & 3 Rail Station", "Hayes & Harlington",
                             "Hanwell", "Southall Station", "West Ealing Station", "Ealing Broadway", "Acton Main Line")

east_crossrail_stations <- c("Stratford Station", "Maryland Station", "Forest Gate Station", "Manor Park Rail Station", 
                             "Ilford", "Seven Kings", "Goodmayes", "Chadwell Heath", 
                             "Romford Station", "Gidea Park", "Harold Wood", "Brentwood Rail Station", 
                             "Woolwich Crossrail Station", "Abbey Wood Station")

central_crossrail_stations <- c("Whitechapel Station", "Bond Street Station", "Tottenham Court Road Station", "Farringdon",
                                "Liverpool Street Station", "Canary Wharf Crossrail Station", "Custom House")

# Concatenate station names into a single string for each area
west_stations <- paste(west_crossrail_stations, collapse = ", ")
east_stations <- paste(east_crossrail_stations, collapse = ", ")
central_stations <- paste(central_crossrail_stations, collapse = ", ")

# Create the summary table
station_table <- data.frame(
  Area = c("West", "East", "Central"),
  Stations = c(west_stations, east_stations, central_stations),
  stringsAsFactors = FALSE
)

# Display the summary table using stargazer
stargazer(station_table, type = "html", summary = FALSE, rownames = FALSE,
          out="station_table.htm")

#summary table

summary_table_df <- properties_with_census

# Load necessary library
library(dplyr)

# Calculate mean and standard deviation for each variable
variables <- c("Price", "closeToCrossrail", "nearestCrossrail", "postTreatment", "undergroundDistance", 
               "populationDensity", "cazDistance", "semiDetachedBinary", 
               "detachedBinary", "flatBinary", "terracedBinary", "avgBedrooms", "qualificationLevel4")


# Manually input descriptions, sources, and units
descriptions <- c(
  "Property price",
  "1 within 1km of Crossrail station, 0 if not",
  "Distance to nearest Crossrail station",
  "1 if post opening, 0 if not",
  "Distance to nearest underground station",
  "Population density in Output Area",
  "Distance to Central Activities Zone",
  "1 if semi-detached house, 0 if not",
  "1 if detached house, 0 if not",
  "1 if a flat, 0 if not",
  "1 if terraced, 0 if not",
  "Average number of bedrooms in Output Area",
  "Proportion with Level 4 qualification or above in Output area"
)

sources <- c(
  "Land Registry",
  "TfL",
  "TfL",
  "Derived from calculations",
  "TfL",
  "Nomis",
  "London Datastore",
  "Land Registry",
  "Land Registry",
  "Land Registry",
  "Land Registry",
  "Nomis",
  "Nomis"
)

units <- c(
  "Log Pounds",
  "Binary",
  "Kilometre",
  "Binary",
  "Log metre",
  "Log density",
  "Log metre",
  "Binary",
  "Binary",
  "Binary",
  "Binary",
  "Number",
  "Number"
)

# Create the summary table
variable_table <- data.frame(
  Variable = variables,
  Description = descriptions,
  Source = sources,
  Unit = units,
  stringsAsFactors = FALSE
)

# Print the summary table using stargazer
stargazer(summary_table, type = "html", summary = FALSE, rownames = FALSE,
          out = "variable_table.htm")

# looking at summary_data, make a table with the mean, median, range, and standard deviation of log_price, closs_to_crossrail, noiseclass, log_underground_distance, log_population_density, log_min_distance_to_caz, flat_binary, semi_detached_binary, avg_bedrooms, and average_qualification_level_4
# Calculate the summary statistics
library(dplyr)
library(stargazer)
library(tidyr)

#find and adapt code from my nice old summary tables

# Select the relevant columns and ensure they are numeric
numeric_columns <- combined_df %>%
  select(log_price_in_2010_pounds, price_in_2010_pounds, 
         close_to_crossrail_network, road_distance_crossrail, 
         noiseclass, log_population_density, 
         avg_bedrooms, average_qualification_level_4,  employment_pct_60, 
         old_new, log_subcentre_distance, travel_time_p50, noiseclass, log_crime_count, access_parks_30_pct, average_qualification_level_4) %>%
  mutate(across(everything(), as.numeric))





library(dplyr)
library(tidyr)

# Function to calculate summary statistics for a specific group
calculate_summary <- function(data) {
  data %>%
    summarise(
      count = n(),
      Mean = round(mean(log_price, na.rm = TRUE), 2),
      Median = round(median(log_price, na.rm = TRUE), 2),
      SD = round(sd(log_price, na.rm = TRUE), 2),
      Min = round(min(log_price, na.rm = TRUE), 2),
      Max = round(max(log_price, na.rm = TRUE), 2)
    )
}

# Calculate summary statistics for close_to_crossrail_network = 1
summary_stats_1 <- numeric_columns %>%
  filter(close_to_crossrail_network == 1) %>%
  summarise_all(list(mean = ~ mean(., na.rm = TRUE),
                     median = ~ median(., na.rm = TRUE),
                     sd = ~ sd(., na.rm = TRUE),
                     min = ~ min(., na.rm = TRUE),
                     max = ~ max(., na.rm = TRUE))) %>%
  mutate(close_to_crossrail_network = 1)

# Calculate summary statistics for close_to_crossrail_network = 0
summary_stats_0 <- numeric_columns %>%
  filter(close_to_crossrail_network == 0) %>%
  summarise_all(list(mean = ~ mean(., na.rm = TRUE),
                     median = ~ median(., na.rm = TRUE),
                     sd = ~ sd(., na.rm = TRUE),
                     min = ~ min(., na.rm = TRUE),
                     max = ~ max(., na.rm = TRUE))) %>%
  mutate(close_to_crossrail_network = 0)

# Combine the two summary tables
final_combined_summary <- bind_rows(summary_stats_1, summary_stats_0) %>%
  pivot_longer(cols = -close_to_crossrail_network, names_to = "Statistic", values_to = "Value") %>%
  pivot_wider(names_from = close_to_crossrail_network, values_from = Value)

# Rename the columns to indicate the group
colnames(final_combined_summary) <- c("Statistic", "Crossrail_0", "Crossrail_1")

# Print the resulting combined data frame
print(final_combined_summary)

#how many 1 in noiseclass in summary_stats_0
subset_crossrail_0 %>%
  filter(noiseclass == 0) %>%
  summarise(count = n())

#percentage of 1 vs percentage of 0 in noiseclass in summary_stats_0
subset_crossrail_0 %>%
  summarise(
    noiseclass_2 = sum(noiseclass == 2, na.rm = TRUE),
    noiseclass_1 = sum(noiseclass == 1, na.rm = TRUE),
    total = n(),
    percentage_2 = noiseclass_2 / total * 100,
    percentage_1 = noiseclass_1 / total * 100
  )


library(dplyr)

# Assuming 'combined_df' is your data frame and it includes columns for 'region', 'property_type', and 'log_price'
library(dplyr)
library(tidyr)

# Calculate overall mean log price, count, and percentage for each region
overall_summary <- combined_df %>%
  group_by(region) %>%
  summarise(
    Mean_Log_Price = mean(price_in_2010_pounds, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  mutate(Percentage = n / sum(n) * 100)

# Print overall summary
print(overall_summary)
# Calculate mean log price, count, and percentage by property type within each region
property_type_summary <- combined_df %>%
  group_by(region, property_type) %>%
  summarise(
    Mean_Log_Price = mean(price_in_2010_pounds, na.rm = TRUE),
    n = n(),
    .groups = 'drop'
  ) %>%
  left_join(overall_summary, by = "region", suffix = c("", "_total")) %>%
  mutate(Percentage = n / n_total * 100) %>%
  select(region, property_type, Mean_Log_Price, n, Percentage) %>%
  pivot_wider(names_from = region, values_from = c(Mean_Log_Price, n, Percentage))

# Print property type summary
print(property_type_summary)
# Add an "Overall" entry to the property type summary for each region
overall_summary_wide <- overall_summary %>%
  pivot_wider(names_from = region, values_from = c(Mean_Log_Price, n, Percentage)) %>%
  mutate(property_type = "Overall")

# Combine the overall summary with the property-type-specific summary
final_summary_table <- bind_rows(overall_summary_wide, property_type_summary)

# Arrange the final table for clarity
final_summary_table <- final_summary_table %>%
  arrange(factor(property_type, levels = c("Overall", "Detached", "Semi-Detached", "Terraced", "Flat")))

# Print the final summary table
print(final_summary_table)
