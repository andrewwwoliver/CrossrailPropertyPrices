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

# looking at summary_data, make a table with the mean, median, range, and standard deviation of log_price, closs_to_crossrail, post_treatment, log_underground_distance, log_population_density, log_min_distance_to_caz, flat_binary, semi_detached_binary, avg_bedrooms, and average_qualification_level_4
# Calculate the summary statistics
library(dplyr)
library(stargazer)
library(tidyr)


# Select the relevant columns and ensure they are numeric
numeric_columns <- summary_data %>%
  select(log_price, close_to_crossrail, nearest_crossrail, post_treatment, log_underground_distance, log_population_density, 
         log_min_distance_to_caz, semi_detached_binary, detached_binary, flat_binary, terraced_binary, avg_bedrooms, average_qualification_level_4) %>%
  mutate(across(everything(), as.numeric))

# Rename columns
colnames(numeric_columns) <- variables

# Calculate the summary statistics using reframe and round values
summary_stats <- numeric_columns %>%
  reframe(
    Variable = names(numeric_columns),
    Mean = round(sapply(numeric_columns, mean, na.rm = TRUE), 2),
    Median = round(sapply(numeric_columns, median, na.rm = TRUE), 2),
    Range = round(sapply(numeric_columns, function(x) diff(range(x, na.rm = TRUE))), 2),
    SD = round(sapply(numeric_columns, sd, na.rm = TRUE), 2)
  )

# Convert summary_stats to long format and then reshape it to wide format
summary_stats_long <- summary_stats %>%
  pivot_longer(cols = -Variable, names_to = "Statistic", values_to = "Value") %>%
  pivot_wider(names_from = Statistic, values_from = Value)


# Display the summary statistics table using stargazer
stargazer(summary_stats_long, type = "html", summary = FALSE, rownames = FALSE,
          out = "summary_stats_table.htm")



# summary for non detached

# exclude D in property type from summary data
non_summary_data <- summary_data %>%
  filter(property_type != "D")

#how many rows are in the non_summary_data excl

# Select the relevant columns and ensure they are numeric
numeric_columns <- non_summary_data %>%
  select(log_price, close_to_crossrail, nearest_crossrail, post_treatment, log_underground_distance, log_population_density, 
         log_min_distance_to_caz, semi_detached_binary,  flat_binary, terraced_binary, avg_bedrooms, average_qualification_level_4) %>%
  mutate(across(everything(), as.numeric))

non_variables <- c("Price", "closeToCrossrail", "nearestCrossrail", "postTreatment", "undergroundDistance", 
               "populationDensity", "cazDistance", "semiDetachedBinary", 
               "flatBinary", "terracedBinary", "avgBedrooms", "qualificationLevel4")

# Rename columns
colnames(numeric_columns) <- non_variables

# Calculate the summary statistics using reframe and round values
summary_stats <- numeric_columns %>%
  reframe(
    Variable = names(numeric_columns),
    Mean = round(sapply(numeric_columns, mean, na.rm = TRUE), 2),
    Median = round(sapply(numeric_columns, median, na.rm = TRUE), 2),
    Range = round(sapply(numeric_columns, function(x) diff(range(x, na.rm = TRUE))), 2),
    SD = round(sapply(numeric_columns, sd, na.rm = TRUE), 2)
  )

# Convert summary_stats to long format and then reshape it to wide format
non_summary_stats_long <- summary_stats %>%
  pivot_longer(cols = -Variable, names_to = "Statistic", values_to = "Value") %>%
  pivot_wider(names_from = Statistic, values_from = Value)


# Display the summary statistics table using stargazer
stargazer(non_summary_stats_long, type = "html", summary = FALSE, rownames = FALSE,
          out = "non_summary_stats_table.htm")

# what is the mean price in summary_table for each property_type 
# Calculate the mean price for each property type
mean_price_by_property_type <- summary_data %>%
  group_by(property_type) %>%
  summarise(Mean_Price = mean(price, na.rm = TRUE))

# Display the mean price by property type table using pritn
print(mean_price_by_property_type)

library(stargazer)
# Create a data frame with the RESET test results
# Create a data frame with the RESET test results
reset_test_results <- data.frame(
  Model = c("Western Detached", "Eastern Detached", "Western Flat", "Eastern Flat",
            "Western Semi-Detached", "Eastern Semi-Detached", "Western Terraced", "Eastern Terraced"),
  RESET = c(28.6, 51.618, 209.75, 440.99, 137.61, 89.877, 445.75, 358.55),
  df1 = c(2, 2, 2, 2, 2, 2, 2, 2),
  df2 = c(7757, 5070, 39941, 48867, 19176, 18620, 31921, 53142),
  p_value = c("4.215e-13", "< 2.2e-16", "< 2.2e-16", "< 2.2e-16", "< 2.2e-16", "< 2.2e-16", "< 2.2e-16", "< 2.2e-16")
)

# Present the table using stargazer
stargazer(reset_test_results, type = "html", summary = FALSE, rownames = FALSE,
          title = "Comparison of RESET Tests for Different Models",
          column.labels = c("Model", "RESET", "df1", "df2", "p-value"),
          out = "reset_test_results.htm")

