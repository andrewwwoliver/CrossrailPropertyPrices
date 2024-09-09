
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

# 2010 CPI prices
# load CPI.RData
load("CPI.RData")

# Merge the CPI data with your existing data
properties_with_census <- merge(properties_with_census, cpi, by.x = "year", by.y = "year", all.x = TRUE)

#drop CPI.y and rename CPI.x to CPI
properties_with_census <- properties_with_census %>%
  select(-CPI.y) %>%
  rename(CPI = CPI.x)

# Calculate the adjustment factor
base_cpi <- cpi$CPI[cpi$year == 2010]
properties_with_census <- properties_with_census %>%
  mutate(adjustment_factor = base_cpi / CPI)

# Adjust the prices to 2010 pounds
properties_with_census <- properties_with_census %>%
  mutate(price_in_2010_pounds = price * adjustment_factor)


#log 2010 pounds (+1 to avoid 0 ) 
properties_with_census <- properties_with_census %>%
  mutate(log_price_in_2010_pounds = log(price_in_2010_pounds + 1))

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
crossrail_start_year <- 2022
properties_with_census <- properties_with_census %>%
  mutate(post_treatment = ifelse(year >= crossrail_start_year, 1, 0))

# interaction network
properties_with_census <- properties_with_census %>%
  mutate(interaction_close_treatment = close_to_crossrail_network * post_treatment)

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

# load stops_all_time
load("stops_all_time.RData")

#filter road_distance_crossrail to 2.5 or less to match eucelidan crop 
properties_with_census <- properties_with_census %>%
  filter(road_distance_crossrail <= 2.5)

# join properties_with_census and stops_all_time by stop_name in stops_all_time to nearest_crossrail_network in properties_with_census
# Perform the join
properties_with_census <- merge(properties_with_census, stops_all_time, 
                                by.x = "nearest_crossrail_network", 
                                by.y = "stop_name", 
                                all.x = TRUE)

# drop sales before 2017
#properties_with_census <- properties_with_census %>%
#  filter(year >= 2017)

#rename N to crossrail_frequency
properties_with_census <- properties_with_census %>%
  rename(crossrail_frequency_n = N)

#divide crossrail_frequency_n by 1000
properties_with_census$crossrail_frequency_n <- properties_with_census$crossrail_frequency_n / 1000

#subset where flat type is F
flat_subset <- properties_with_census %>%
  filter(property_type == "F")

#filter out extreme values in log_price
lower_bound_price <- quantile(flat_subset$log_price, 0.05)
upper_bound_price <- quantile(flat_subset$log_price, 0.95)

# semi-detached subset
semi_detached_subset <- properties_with_census %>%
  filter(property_type == "S")

#filter out extreme values in log_price
lower_bound_price <- quantile(semi_detached_subset$log_price, 0.05)
upper_bound_price <- quantile(semi_detached_subset$log_price, 0.95)

# detached subset
detached_subset <- properties_with_census %>%
  filter(property_type == "D")

#filter out extreme values in log_price
lower_bound_price <- quantile(detached_subset$log_price, 0.05)
upper_bound_price <- quantile(detached_subset$log_price, 0.95)

# terraced subset
terraced_subset <- properties_with_census %>%
  filter(property_type == "T")


#filter out extreme values in log_price
lower_bound_price <- quantile(terraced_subset$log_price, 0.05)
upper_bound_price <- quantile(terraced_subset$log_price, 0.95)


# make summary stats dataset
## need new summary without the filtered ones
#summary_data <- properties_with_census


library(lme4)
# could anova to compare models? as in https://stats.stackexchange.com/questions/594553/how-to-interpret-the-output-of-a-generalized-linear-model-with-r-lmer

library(lme4)
library(lmerTest)
#combine flat detached semi-detached and terraced subsets (long ways) flat_subset, detached_subset, semi_detached_subset, terraced_subset
combined_df <- rbind(flat_subset, detached_subset, semi_detached_subset, terraced_subset)


#  random slope model: allowing predictors to have varying slopes across LSOAs
multilevel_model <- lmer(log_price_in_2010_pounds ~ 
                           close_to_crossrail_network * post_treatment +  
                           log_population_density + 
                           avg_bedrooms + 
                           average_qualification_level_4 + 
                           employment_pct_60 + 
                           old_new +
                           log_subcentre_distance +
                          # travel_time_p50 +
                           property_type +
                           noiseclass +
                           log_crime_count +
                           access_parks_30_pct +
                           (1  + post_treatment | LSOA11CD) ,
                         data = combined_df)
# Summary of the model
summary(multilevel_model)

ranova(multilevel_model)
library(lmtest)
# Extract residuals from the model
residuals <- residuals(multilevel_model)

# Perform the Durbin-Watson test on the residuals
dw_test <- dwtest(residuals ~ 1)

# View the test results
print(dw_test)


#lm model for crplots
lm_whole_model <-  lm(log_price_in_2010_pounds ~ 
                             close_to_crossrail_network + post_treatment +  
                             log_population_density + 
                             avg_bedrooms + 
                             average_qualification_level_4 + 
                             employment_pct_60 + 
                             old_new +
                             log_subcentre_distance +
                               property_type +
                             # travel_time_p50 +
                             noiseclass +
                             log_crime_count +
                             access_parks_30_pct,# +
                            # (1  + post_treatment | LSOA11CD) ,
                           data = flat_Subset)
# Summary of the model
summary(lm_flat_model)



# crplots 
library(car)
crPlots(lm_flat_model)
#confint(multilevel_model)




#coifs

# multicollinearity test
vif(multilevel_model)



detached_lmer_model <- lmer(log_price_in_2010_pounds ~ 
                              close_to_crossrail_network * post_treatment +  
                              log_population_density + 
                              avg_bedrooms + 
                              average_qualification_level_4 + 
                               
                              employment_pct_60 + 
                              old_new +
                              log_subcentre_distance +
                              # travel_time_p50 +
                              noiseclass +
                              log_crime_count +
                              access_parks_30_pct +
                              (1 + post_treatment | LSOA11CD),
                            data = detached_subset)

flat_lmer_model <- lmer(log_price_in_2010_pounds ~ 
                          close_to_crossrail_network * post_treatment +  
                          log_population_density + 
                          avg_bedrooms + 
                          average_qualification_level_4 + 
                           
                          employment_pct_60 + 
                          old_new +
                          log_subcentre_distance +
                          # travel_time_p50 +
                          noiseclass +
                          log_crime_count +
                          access_parks_30_pct +
                          (1 + post_treatment | LSOA11CD),
                              data = flat_subset)

summary(flat_lmer_model)
#confint(flat_lmer_model)



semi_detached_lmer_model <- lmer(log_price_in_2010_pounds ~ 
                                   close_to_crossrail_network * post_treatment +  
                                   log_population_density + 
                                   avg_bedrooms + 
                                   average_qualification_level_4 + 
                                    
                                   employment_pct_60 + 
                                   old_new +
                                   log_subcentre_distance +
                                   # travel_time_p50 +
                                   noiseclass +
                                   log_crime_count +
                                   access_parks_30_pct +
                                   (1 + post_treatment | LSOA11CD),
                                       data = semi_detached_subset)

terraced_lmer_model <- lmer(log_price_in_2010_pounds ~ 
                              close_to_crossrail_network * post_treatment +  
                              log_population_density + 
                              avg_bedrooms + 
                              average_qualification_level_4 + 
                               
                              employment_pct_60 + 
                              old_new +
                              log_subcentre_distance +
                              # travel_time_p50 +
                              noiseclass +
                              log_crime_count +
                              access_parks_30_pct +
                              (1 + post_treatment | LSOA11CD),
                                  data = terraced_subset)

# subset detached_subset for property_type in west_crossrail_stations
western_detached_subset <- subset(detached_subset, nearest_station %in% west_crossrail_stations)
eastern_detached_subset <- subset(detached_subset, nearest_station %in% east_crossrail_stations)
central_detached_subset <- subset(detached_subset, nearest_station %in% central_crossrail_stations)

# subset flat_subset for property_type in west_crossrail_stations
western_flat_subset <- subset(flat_subset, nearest_station %in% west_crossrail_stations)
eastern_flat_subset <- subset(flat_subset, nearest_station %in% east_crossrail_stations)
central_flat_subset <- subset(flat_subset, nearest_station %in% central_crossrail_stations)

# subset semi_detached_subset for property_type in west_crossrail_stations
western_semi_detached_subset <- subset(semi_detached_subset, nearest_station %in% west_crossrail_stations)
eastern_semi_detached_subset <- subset(semi_detached_subset, nearest_station %in% east_crossrail_stations)
central_semi_detached_subset <- subset(semi_detached_subset, nearest_station %in% central_crossrail_stations)

# subset terraced_subset for property_type in west_crossrail_stations
western_terraced_subset <- subset(terraced_subset, nearest_station %in% west_crossrail_stations)
eastern_terraced_subset <- subset(terraced_subset, nearest_station %in% east_crossrail_stations)
central_terraced_subset <- subset(terraced_subset, nearest_station %in% central_crossrail_stations)

  
western_detached_lmer_model <- lmer(log_price_in_2010_pounds ~ 
                                      close_to_crossrail_network * post_treatment +  
                                      log_population_density + 
                                      avg_bedrooms + 
                                      average_qualification_level_4 + 
                                       
                                      employment_pct_60 + 
                                      old_new +
                                      log_subcentre_distance +
                                      # travel_time_p50 +
                                      noiseclass +
                                      log_crime_count +
                                      access_parks_30_pct +
                                      (1 + post_treatment | LSOA11CD),
                                    data = western_detached_subset)

eastern_detached_lmer_model <- lmer(log_price_in_2010_pounds ~ 
                                      road_distance_crossrail * post_treatment +  
                                      log_population_density + 
                                      avg_bedrooms + 
                                      average_qualification_level_4 + 
                                       
                                      employment_pct_60 + 
                                      old_new +
                                      log_subcentre_distance +
                                      # travel_time_p50 +
                                      noiseclass +
                                      log_crime_count +
                                      access_parks_30_pct +
                                      (1 + post_treatment | LSOA11CD),
                                    data = eastern_detached_subset)

#summary
#unusable due to small n 
central_detached_lmer_model <- lmer(log_price_in_2010_pounds ~ 
                                      close_to_crossrail_network * post_treatment +  
                                      log_population_density + 
                                      avg_bedrooms + 
                                      average_qualification_level_4 + 
                                       
                                      employment_pct_60 + 
                                      old_new +
                                      log_subcentre_distance +
                                      # travel_time_p50 +
                                      noiseclass +
                                      log_crime_count +
                                      access_parks_30_pct +
                                      (1 #+ post_treatment 
                                       | LSOA11CD),
                                    data = central_detached_subset)


# subset eastern_subset to only include F proeprty type
eastern_subset_f <- eastern_flat_subset
# filter for year 2016 and above
#eastern_subset_f <- eastern_subset_f %>%
#  filter(year >= 2020)



eastern_flat_lmer_model <- lmer(log_price_in_2010_pounds ~ 
                                  close_to_crossrail_network * post_treatment +  
                                  log_population_density + 
                                  avg_bedrooms + 
                                  average_qualification_level_4 + 
                                   
                                  employment_pct_60 + 
                                  old_new +
                                  log_subcentre_distance +
                                  # travel_time_p50 +
                                  noiseclass +
                                  log_crime_count +
                                  access_parks_30_pct +
                                  (1 + post_treatment | LSOA11CD),
                              data = eastern_subset_f)

#summary
summary(eastern_flat_lmer_model)

confint(eastern_flat_lmer_model)

# multicollinearity test
vif(eastern_flat_lmer_model)
#make year a factor in eastern_subset_f
eastern_subset_f$year <- as.factor(eastern_subset_f$year)


eastern_flat_lm_model <- lm(log_price_in_2010_pounds ~ close_to_crossrail_network * post_treatment +
                              log_population_density + 
                              avg_bedrooms + 
                              average_qualification_level_4 + 
                               
                              employment_pct_60 + 
                              old_new +
                              log_subcentre_distance +
                              #    travel_time_p50 +
                              noiseclass +
                              log_crime_count +
                              access_parks_30_pct,
                            data = flat_subset)


#summary
summary(eastern_flat_lm_model)

vif(eastern_flat_lm_model)


library(ggplot2)

# Assuming 'eastern_subset_f' is your dataset and 'nearest_crossrail_network' is a variable in it
# new column called area where nearest_crossrail_network is grouped by eastern / western / central
combined_df$area <- ifelse(combined_df$nearest_crossrail_network %in% east_crossrail_stations, "Eastern", 
                                ifelse(combined_df$nearest_crossrail_network %in% west_crossrail_stations, "Western", "Central"))

ggplot(data = combined_df, 
       aes(x   = road_distance_crossrail,  
           y   = log_price_in_2010_pounds,                
           col = as.factor(area))) +
  geom_point(size     = 0.5,   # Reduce point size
             alpha    = 0.02,   # Increase transparency
             position = "jitter") +
  geom_smooth(method   = lm,
              se       = FALSE,  # Turn off confidence intervals
              size     = 1.3,    # Slightly reduce line size
              linetype = 1, 
              alpha    = 1) +
  theme_minimal() +
  scale_color_manual(name = "Nearest Crossrail Station", 
                     values = c("Central" = "red", "Eastern" = "blue", "Western" = "orange")) +  # Custom colors for better visibility
  theme(legend.position = "right")  # Adjust legend position if needed
#rename T S F AND D to Terraced, Semi-detached, Flat, Detached
combined_df$property_type <- ifelse(combined_df$property_type == "T", "Terraced", 
                                    ifelse(combined_df$property_type == "S", "Semi-detached", 
                                           ifelse(combined_df$property_type == "F", "Flat", "Detached")))
ggplot(data = combined_df, 
       aes(x   = road_distance_crossrail,  
           y   = log_price_in_2010_pounds,                
           col = as.factor(property_type))) +
  geom_point(size     = 0.5,   # Reduce point size
             alpha    = 0.02,   # Increase transparency
             position = "jitter") +
  geom_smooth(method   = lm,
              se       = FALSE,  # Turn off confidence intervals
              size     = 1.3,    # Slightly reduce line size
              linetype = 1, 
              alpha    = 1) +
  theme_minimal() +
  scale_color_manual(name = "Property Type", 
                     values = c("Terraced" = "red", "Semi-detached" = "blue", "Flat" = "orange", "Detached" = "purple")) +  # Custom colors for better visibility
  theme(legend.position = "right")  # Adjust legend position if needed



library(ggplot2)
tdat <- data.frame(predicted=predict(eastern_flat_lmer_model), residual=residuals(eastern_flat_lmer_model), road_distance_crossrail = eastern_subset_f$road_distance_crossrail)
ggplot(tdat,aes(x=predicted,y=residual, colour = road_distance_crossrail)) + geom_point() + geom_smooth()


#crPlots(eastern_flat_lm_model)


western_flat_lmer_model <- lmer(log_price_in_2010_pounds ~ 
                                  close_to_crossrail_network * post_treatment +  
                                  log_population_density + 
                                  avg_bedrooms + 
                                  average_qualification_level_4 + 
                                   
                                  employment_pct_60 + 
                                  old_new +
                                  log_subcentre_distance +
                                  # travel_time_p50 +
                                  noiseclass +
                                  log_crime_count +
                                  access_parks_30_pct +
                                  (1 + post_treatment | LSOA11CD),
                                data = western_flat_subset)
summary(western_flat_lmer_model)

western_flat_travel_model <- lmer(log_price_in_2010_pounds ~ 
                                  close_to_crossrail_network * post_treatment +  
                                  log_population_density + 
                                  avg_bedrooms + 
                                  average_qualification_level_4 + 
                                  
                                #  employment_pct_60 + 
                                  old_new +
                                  log_subcentre_distance +
                                   travel_time_p50 +
                                  noiseclass +
                                  log_crime_count +
                                  access_parks_30_pct +
                                  (1 + post_treatment | LSOA11CD),
                                data = western_flat_subset)
summary(western_flat_travel_model)

western_flat_notravel_model <- lmer(log_price_in_2010_pounds ~ 
                                    close_to_crossrail_network * post_treatment +  
                                    log_population_density + 
                                    avg_bedrooms + 
                                    average_qualification_level_4 + 
                                    
                                    #  employment_pct_60 + 
                                    old_new +
                                   # log_subcentre_distance +
                                   # travel_time_p50 +
                                    noiseclass +
                                    log_crime_count +
                                    access_parks_30_pct +
                                    (1 + post_treatment | LSOA11CD),
                                  data = western_flat_subset)
summary(western_flat_notravel_model)
# when both included
vif(western_flat_travel_model)

#confint(western_flat_lmer_model)
western_semi_detached_lmer_model <- lmer(log_price_in_2010_pounds ~ 
                                           close_to_crossrail_network * post_treatment +  
                                           log_population_density + 
                                           avg_bedrooms + 
                                           average_qualification_level_4 + 
                                            
                                           employment_pct_60 + 
                                           old_new +
                                           log_subcentre_distance +
                                           # travel_time_p50 +
                                           noiseclass +
                                           log_crime_count +
                                           access_parks_30_pct +
                                           (1 + post_treatment | LSOA11CD),
                                         data = western_semi_detached_subset)

eastern_semi_detached_lmer_model <- lmer(log_price_in_2010_pounds ~ 
                                           close_to_crossrail_network * post_treatment +  
                                           log_population_density + 
                                           avg_bedrooms + 
                                           average_qualification_level_4 + 
                                            
                                           employment_pct_60 + 
                                           old_new +
                                           log_subcentre_distance +
                                           # travel_time_p50 +
                                           noiseclass +
                                           log_crime_count +
                                           access_parks_30_pct +
                                           (1 + post_treatment | LSOA11CD),
                                         data = eastern_semi_detached_subset)

western_terraced_lmer_model <- lmer(log_price_in_2010_pounds ~ 
                                      close_to_crossrail_network * post_treatment +  
                                      log_population_density + 
                                      avg_bedrooms + 
                                      average_qualification_level_4 + 
                                       
                                      employment_pct_60 + 
                                      old_new +
                                      log_subcentre_distance +
                                      # travel_time_p50 +
                                      noiseclass +
                                      log_crime_count +
                                      access_parks_30_pct +
                                      (1 + post_treatment | LSOA11CD),
                                    data = western_terraced_subset)

eastern_terraced_lmer_model <- lmer(log_price_in_2010_pounds ~ 
                                      close_to_crossrail_network * post_treatment +  
                                      log_population_density + 
                                      avg_bedrooms + 
                                      average_qualification_level_4 + 
                                       
                                      employment_pct_60 + 
                                      old_new +
                                      log_subcentre_distance +
                                      # travel_time_p50 +
                                      noiseclass +
                                      log_crime_count +
                                      access_parks_30_pct +
                                      (1 + post_treatment | LSOA11CD),
                                    data = eastern_terraced_subset)

central_flat_lmer_model <- lmer(log_price_in_2010_pounds ~ 
                                  close_to_crossrail_network * post_treatment +  
                                  log_population_density + 
                                  avg_bedrooms + 
                                  average_qualification_level_4 + 
                                   
                                  employment_pct_60 + 
                                  old_new +
                                  log_subcentre_distance +
                                  # travel_time_p50 +
                                  noiseclass +
                                  log_crime_count +
                                  access_parks_30_pct +
                                  (1 + post_treatment | LSOA11CD),
                                data = central_flat_subset)
#confint(central_flat_lmer_model)
summary(central_flat_lmer_model)

central_semi_detached_lmer_model <- lmer(log_price_in_2010_pounds ~ 
                                           close_to_crossrail_network * post_treatment +  
                                           log_population_density + 
                                           avg_bedrooms + 
                                           average_qualification_level_4 + 
                                            
                                           employment_pct_60 + 
                                           old_new +
                                           log_subcentre_distance +
                                           # travel_time_p50 +
                                           noiseclass +
                                           log_crime_count +
                                           access_parks_30_pct +
                                           (1 + post_treatment | LSOA11CD),
                                         data = central_semi_detached_subset)

central_terraced_lmer_model <- lmer(log_price_in_2010_pounds ~ 
                                      close_to_crossrail_network * post_treatment +  
                                      log_population_density + 
                                      avg_bedrooms + 
                                      average_qualification_level_4 + 
                                       
                                      employment_pct_60 + 
                                      old_new +
                                      log_subcentre_distance +
                                      # travel_time_p50 +
                                      noiseclass +
                                      log_crime_count +
                                      access_parks_30_pct +
                                      (1 + post_treatment | LSOA11CD),
                                    data = central_terraced_subset)

central_terraced_travel_model <- lmer(log_price_in_2010_pounds ~ 
                                      close_to_crossrail_network * post_treatment +  
                                      log_population_density + 
                                      avg_bedrooms + 
                                      average_qualification_level_4 + 
                                      
                                      #employment_pct_60 + 
                                      old_new +
                                      log_subcentre_distance +
                                       travel_time_p50 +
                                      noiseclass +
                                      log_crime_count +
                                      access_parks_30_pct +
                                      (1 + post_treatment | LSOA11CD),
                                    data = central_terraced_subset)

# Save all models to a single RData file
save(multilevel_model, 
     detached_lmer_model, flat_lmer_model, semi_detached_lmer_model, terraced_lmer_model,
     western_detached_lmer_model, eastern_detached_lmer_model, central_detached_lmer_model,
     western_flat_lmer_model, eastern_flat_lmer_model, western_semi_detached_lmer_model, eastern_semi_detached_lmer_model,
     western_terraced_lmer_model, eastern_terraced_lmer_model, central_flat_lmer_model, central_semi_detached_lmer_model,
     central_terraced_lmer_model, file = "all_lmer_models.RData")


library(Matrix)
library(spatialreg)
library(spdep)
library(sf)
library(sp)

# MULTILEVEL MODEL
properties_with_census_sf <- st_sf(properties_with_census)
st_crs(properties_with_census_sf) <- 4326
set.seed(123)
multilevel_sample <- properties_with_census_sf[sample(nrow(properties_with_census_sf), size = 10000), ]
multilevel_sample <- st_cast(multilevel_sample, "POINT")
coords <- st_coordinates(multilevel_sample)
coords_jittered <- coords + matrix(rnorm(n = length(coords), mean = 0, sd = 1e-4), ncol = 2)
multilevel_sample$geometry <- st_sfc(lapply(1:nrow(coords_jittered), function(i) {
  st_point(coords_jittered[i, ])
}), crs = st_crs(multilevel_sample))

multilevel_sampled_model <- lm(log_price_in_2010_pounds ~ 
                                 close_to_crossrail_network * post_treatment +  
                                 log_population_density + 
                                 avg_bedrooms + 
                                 average_qualification_level_4 + 
                                  
                                 employment_pct_60 + 
                                 old_new +
                                 log_subcentre_distance +
                                 # travel_time_p50 +
                                 noiseclass +
                                 log_crime_count +
                                 access_parks_30_pct,
                               data = multilevel_sample)

residuals <- residuals(multilevel_sampled_model)
nb <- knn2nb(knearneigh(coords_jittered, k = 8))
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)
moran_test <- moran.test(residuals, lw)
print(moran_test)

multilevel_slm <- lagsarlm(log_price_in_2010_pounds ~ 
                             close_to_crossrail_network * post_treatment +  
                             log_population_density + 
                             avg_bedrooms + 
                             average_qualification_level_4 + 
                              
                             employment_pct_60 + 
                             old_new +
                             log_subcentre_distance +
                             property_type +
                             # travel_time_p50 +
                             noiseclass +
                             log_crime_count +
                             access_parks_30_pct, 
                           data = multilevel_sample, listw = lw)

save(multilevel_slm, file = "multilevel_slm.RData")

summary(multilevel_slm)

#  random slope model: allowing predictors to have varying slopes across LSOAs
multilevel_sample_model <- lmer(log_price_in_2010_pounds ~ 
                                  close_to_crossrail_network * post_treatment +  
                                  log_population_density + 
                                  avg_bedrooms + 
                                  average_qualification_level_4 + 
                                  
                                  employment_pct_60 + 
                                  old_new +
                                  log_subcentre_distance +
                                  # travel_time_p50 +
                                  property_type +
                                  noiseclass +
                                  log_crime_count +
                                  access_parks_30_pct +
                                  (1  + post_treatment | LSOA11CD) ,
                                data = multilevel_sample)
# Summary of the model
summary(multilevel_sample_model)

# DETACHED PROPERTIES MODEL
detached_subset <- detached_subset
detached_sf <- st_sf(detached_subset)
st_crs(detached_sf) <- 4326
set.seed(123)
detached_sample <- detached_sf
detached_sample <- st_cast(detached_sample, "POINT")
coords <- st_coordinates(detached_sample)
coords_jittered <- coords + matrix(rnorm(n = length(coords), mean = 0, sd = 1e-4), ncol = 2)
detached_sample$geometry <- st_sfc(lapply(1:nrow(coords_jittered), function(i) {
  st_point(coords_jittered[i, ])
}), crs = st_crs(detached_sample))

detached_sampled_model <- lm(log_price_in_2010_pounds ~ 
                               close_to_crossrail_network * post_treatment +  
                               log_population_density + 
                               avg_bedrooms + 
                               average_qualification_level_4 + 
                                
                               employment_pct_60 + 
                               old_new +
                               log_subcentre_distance +
                               # travel_time_p50 +
                               noiseclass +
                               log_crime_count +
                               access_parks_30_pct, 
                             data = detached_sample)

residuals <- residuals(detached_sampled_model)
nb <- knn2nb(knearneigh(coords_jittered, k = 8))
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)
moran_test <- moran.test(residuals, lw)
print(moran_test)

detached_slm <- lagsarlm(log_price_in_2010_pounds ~ 
                           close_to_crossrail_network * post_treatment +  
                           log_population_density + 
                           avg_bedrooms + 
                           average_qualification_level_4 + 
                            
                           employment_pct_60 + 
                           old_new +
                           log_subcentre_distance +
                           # travel_time_p50 +
                           noiseclass +
                           log_crime_count +
                           access_parks_30_pct,
                         data = detached_sample, listw = lw)

save(detached_slm, file = "detached_slm.RData")
summary(detached_slm)

#  random slope model: allowing predictors to have varying slopes across LSOAs
detached_sample_model <- lmer(log_price_in_2010_pounds ~ 
                                  close_to_crossrail_network * post_treatment +  
                                  log_population_density + 
                                  avg_bedrooms + 
                                  average_qualification_level_4 + 
                                  employment_pct_60 + 
                                  old_new +
                                  log_subcentre_distance +
                                  # travel_time_p50 +
                                  noiseclass +
                                  log_crime_count +
                                  access_parks_30_pct +
                                  (1  + post_treatment | LSOA11CD) ,
                                data = detached_sample)
# Summary of the model
summary(detached_sample_model)

# WHOLE FLAT MODEL
flat_subset <- flat_subset
flat_sf <- st_sf(flat_subset)
st_crs(flat_sf) <- 4326
set.seed(123)
flat_sample <- flat_sf[sample(nrow(flat_sf), size = 10000), ]
flat_sample <- st_cast(flat_sample, "POINT")
coords <- st_coordinates(flat_sample)
coords_jittered <- coords + matrix(rnorm(n = length(coords), mean = 0, sd = 1e-4), ncol = 2)
flat_sample$geometry <- st_sfc(lapply(1:nrow(coords_jittered), function(i) {
  st_point(coords_jittered[i, ])
}), crs = st_crs(flat_sample))

flat_sampled_model <- lm(log_price_in_2010_pounds ~ 
                           close_to_crossrail_network * post_treatment +  
                           log_population_density + 
                           avg_bedrooms + 
                           average_qualification_level_4 + 
                            
                           employment_pct_60 + 
                           old_new +
                           log_subcentre_distance +
                           # travel_time_p50 +
                           noiseclass +
                           log_crime_count +
                           access_parks_30_pct,
                         data = flat_sample)

residuals <- residuals(flat_sampled_model)
nb <- knn2nb(knearneigh(coords_jittered, k = 8))
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)
moran_test <- moran.test(residuals, lw)
print(moran_test)

flat_slm <- lagsarlm(log_price_in_2010_pounds ~ 
                       close_to_crossrail_network * post_treatment +  
                       log_population_density + 
                       avg_bedrooms + 
                       average_qualification_level_4 + 
                        
                       employment_pct_60 + 
                       old_new +
                       log_subcentre_distance +
                       # travel_time_p50 +
                       noiseclass +
                       log_crime_count +
                       access_parks_30_pct,
                     data = flat_sample, listw = lw)

save(flat_slm, file = "flat_slm.RData")
confint(flat_slm)
summary(flat_slm)

#  random slope model: allowing predictors to have varying slopes across LSOAs
flat_sample_model <- lmer(log_price_in_2010_pounds ~ 
                                close_to_crossrail_network * post_treatment +  
                                log_population_density + 
                                avg_bedrooms + 
                                average_qualification_level_4 + 
                                employment_pct_60 + 
                                old_new +
                                log_subcentre_distance +
                                # travel_time_p50 +
                                noiseclass +
                                log_crime_count +
                                access_parks_30_pct +
                                (1  + post_treatment | LSOA11CD) ,
                              data = flat_sample)
# Summary of the model
summary(flat_sample_model)

# WHOLE SEMI-DETACHED MODEL
semi_detached_subset <- semi_detached_subset
semi_detached_sf <- st_sf(semi_detached_subset)
st_crs(semi_detached_sf) <- 4326
set.seed(123)
semi_detached_sample <- semi_detached_sf[sample(nrow(semi_detached_sf), size = 10000), ]
semi_detached_sample <- st_cast(semi_detached_sample, "POINT")
coords <- st_coordinates(semi_detached_sample)
coords_jittered <- coords + matrix(rnorm(n = length(coords), mean = 0, sd = 1e-4), ncol = 2)
semi_detached_sample$geometry <- st_sfc(lapply(1:nrow(coords_jittered), function(i) {
  st_point(coords_jittered[i, ])
}), crs = st_crs(semi_detached_sample))

semi_detached_sampled_model <- lm(log_price_in_2010_pounds ~ 
                                    close_to_crossrail_network * post_treatment +  
                                    log_population_density + 
                                    avg_bedrooms + 
                                    average_qualification_level_4 + 
                                     
                                    employment_pct_60 + 
                                    old_new +
                                    log_subcentre_distance +
                                    # travel_time_p50 +
                                    noiseclass +
                                    log_crime_count +
                                    access_parks_30_pct,
                                  data = semi_detached_sample)

residuals <- residuals(semi_detached_sampled_model)
nb <- knn2nb(knearneigh(coords_jittered, k = 8))
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)
moran_test <- moran.test(residuals, lw)
print(moran_test)

semi_detached_slm <- lagsarlm(log_price_in_2010_pounds ~ 
                                close_to_crossrail_network * post_treatment +  
                                log_population_density + 
                                avg_bedrooms + 
                                average_qualification_level_4 + 
                                 
                                employment_pct_60 + 
                                old_new +
                                log_subcentre_distance +
                                # travel_time_p50 +
                                noiseclass +
                                log_crime_count +
                                access_parks_30_pct,
                              data = semi_detached_sample, listw = lw)

save(semi_detached_slm, file = "semi_detached_slm.RData")
summary(semi_detached_slm)

#  random slope model: allowing predictors to have varying slopes across LSOAs
semi_detached_sample_model <- lmer(log_price_in_2010_pounds ~ 
                                close_to_crossrail_network * post_treatment +  
                                log_population_density + 
                                avg_bedrooms + 
                                average_qualification_level_4 + 
                                employment_pct_60 + 
                                old_new +
                                log_subcentre_distance +
                                # travel_time_p50 +
                                noiseclass +
                                log_crime_count +
                                access_parks_30_pct +
                                (1  + post_treatment | LSOA11CD) ,
                              data = semi_detached_sample)
# Summary of the model
summary(semi_detached_sample_model)

# WHOLE TERRACED MODEL
terraced_subset <- terraced_subset
terraced_sf <- st_sf(terraced_subset)
st_crs(terraced_sf) <- 4326
set.seed(123)
terraced_sample <- terraced_sf[sample(nrow(terraced_sf), size = 10000), ]
terraced_sample <- st_cast(terraced_sample, "POINT")
coords <- st_coordinates(terraced_sample)
coords_jittered <- coords + matrix(rnorm(n = length(coords), mean = 0, sd = 1e-4), ncol = 2)
terraced_sample$geometry <- st_sfc(lapply(1:nrow(coords_jittered), function(i) {
  st_point(coords_jittered[i, ])
}), crs = st_crs(terraced_sample))

terraced_sampled_model <- lm(log_price_in_2010_pounds ~ 
                               close_to_crossrail_network * post_treatment +  
                               log_population_density + 
                               avg_bedrooms + 
                               average_qualification_level_4 + 
                                
                               employment_pct_60 + 
                               old_new +
                               log_subcentre_distance +
                               # travel_time_p50 +
                               noiseclass +
                               log_crime_count +
                               access_parks_30_pct,
                             data = terraced_sample)

residuals <- residuals(terraced_sampled_model)
nb <- knn2nb(knearneigh(coords_jittered, k = 8))
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)
moran_test <- moran.test(residuals, lw)
print(moran_test)

terraced_slm <- lagsarlm(log_price_in_2010_pounds ~ 
                           close_to_crossrail_network * post_treatment +  
                           log_population_density + 
                           avg_bedrooms + 
                           average_qualification_level_4 + 
                            
                           employment_pct_60 + 
                           old_new +
                           log_subcentre_distance +
                           # travel_time_p50 +
                           noiseclass +
                           log_crime_count +
                           access_parks_30_pct,
                         data = terraced_sample, listw = lw)

save(terraced_slm, file = "terraced_slm.RData")

summary(terraced_slm)
summary(terraced_lmer_model)

#  random slope model: allowing predictors to have varying slopes across LSOAs
terraced_sample_model <- lmer(log_price_in_2010_pounds ~ 
                                close_to_crossrail_network * post_treatment +  
                                log_population_density + 
                                avg_bedrooms + 
                                average_qualification_level_4 + 
                                employment_pct_60 + 
                                old_new +
                                log_subcentre_distance +
                                # travel_time_p50 +
                                noiseclass +
                                log_crime_count +
                                access_parks_30_pct +
                                (1  + post_treatment | LSOA11CD) ,
                              data = terraced_sample)
# Summary of the model
summary(terraced_sample_model)

# WESTERN DETACHED MODEL
western_detached_sf <- st_sf(western_detached_subset)
st_crs(western_detached_sf) <- 4326
set.seed(123)
western_detached_sample <- western_detached_sf
western_detached_sample <- st_cast(western_detached_sample, "POINT")
coords <- st_coordinates(western_detached_sample)
coords_jittered <- coords + matrix(rnorm(n = length(coords), mean = 0, sd = 1e-4), ncol = 2)
western_detached_sample$geometry <- st_sfc(lapply(1:nrow(coords_jittered), function(i) {
  st_point(coords_jittered[i, ])
}), crs = st_crs(western_detached_sample))

western_detached_sampled_model <- lm(log_price_in_2010_pounds ~ 
                                       close_to_crossrail_network * post_treatment +  
                                       log_population_density + 
                                       avg_bedrooms + 
                                       average_qualification_level_4 + 
                                        
                                       employment_pct_60 + 
                                       old_new +
                                       log_subcentre_distance +
                                       # travel_time_p50 +
                                       noiseclass +
                                       log_crime_count +
                                       access_parks_30_pct,
                                     data = western_detached_sample)

residuals <- residuals(western_detached_sampled_model)
nb <- knn2nb(knearneigh(coords_jittered, k = 8))
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)
moran_test <- moran.test(residuals, lw)
print(moran_test)

western_detached_slm <- lagsarlm(log_price_in_2010_pounds ~ 
                                   close_to_crossrail_network * post_treatment +  
                                   log_population_density + 
                                   avg_bedrooms + 
                                   average_qualification_level_4 + 
                                    
                                   employment_pct_60 + 
                                   old_new +
                                   log_subcentre_distance +
                                   # travel_time_p50 +
                                   noiseclass +
                                   log_crime_count +
                                   access_parks_30_pct,
                                 data = western_detached_sample, listw = lw)

save(western_detached_slm, file = "western_detached_slm.RData")

#  random slope model: allowing predictors to have varying slopes across LSOAs
western_detached_sample_model <- lmer(log_price_in_2010_pounds ~ 
                            close_to_crossrail_network * post_treatment +  
                            log_population_density + 
                            avg_bedrooms + 
                            average_qualification_level_4 + 
                            employment_pct_60 + 
                            old_new +
                            log_subcentre_distance +
                            # travel_time_p50 +
                            noiseclass +
                            log_crime_count +
                            access_parks_30_pct +
                            (1  + post_treatment | LSOA11CD) ,
                          data = western_detached_sample)
# Summary of the model
summary(western_detached_sample_model)

# EASTERN DETACHED MODEL
eastern_detached_sf <- st_sf(eastern_detached_subset)
st_crs(eastern_detached_sf) <- 4326
set.seed(123)
eastern_detached_sample <- eastern_detached_sf
eastern_detached_sample <- st_cast(eastern_detached_sample, "POINT")
coords <- st_coordinates(eastern_detached_sample)
coords_jittered <- coords + matrix(rnorm(n = length(coords), mean = 0, sd = 1e-4), ncol = 2)
eastern_detached_sample$geometry <- st_sfc(lapply(1:nrow(coords_jittered), function(i) {
  st_point(coords_jittered[i, ])
}), crs = st_crs(eastern_detached_sample))

eastern_detached_sampled_model <- lm(log_price_in_2010_pounds ~ 
                                       close_to_crossrail_network * post_treatment +  
                                       log_population_density + 
                                       avg_bedrooms + 
                                       average_qualification_level_4 + 
                                        
                                       employment_pct_60 + 
                                       old_new +
                                       log_subcentre_distance +
                                       # travel_time_p50 +
                                       noiseclass +
                                       log_crime_count +
                                       access_parks_30_pct,
                                     data = eastern_detached_sample)

residuals <- residuals(eastern_detached_sampled_model)
nb <- knn2nb(knearneigh(coords_jittered, k = 8))
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)
moran_test <- moran.test(residuals, lw)
print(moran_test)

eastern_detached_slm <- lagsarlm(log_price_in_2010_pounds ~ 
                                   close_to_crossrail_network * post_treatment +  
                                   log_population_density + 
                                   avg_bedrooms + 
                                   average_qualification_level_4 + 
                                    
                                   employment_pct_60 + 
                                   old_new +
                                   log_subcentre_distance +
                                   # travel_time_p50 +
                                   noiseclass +
                                   log_crime_count +
                                   access_parks_30_pct,
                                 data = eastern_detached_sample, listw = lw)

save(eastern_detached_slm, file = "eastern_detached_slm.RData")
summary(eastern_detached_slm)

#  random slope model: allowing predictors to have varying slopes across LSOAs
eastern_detached_sample_model <- lmer(log_price_in_2010_pounds ~ 
                            close_to_crossrail_network * post_treatment +  
                            log_population_density + 
                            avg_bedrooms + 
                            average_qualification_level_4 + 
                            employment_pct_60 + 
                            old_new +
                            log_subcentre_distance +
                            # travel_time_p50 +
                            noiseclass +
                            log_crime_count +
                            access_parks_30_pct +
                            (1  + post_treatment | LSOA11CD) ,
                          data = eastern_detached_sample)
# Summary of the model
summary(eastern_detached_sample_model)



# WESTERN FLAT MODEL
western_flat_sf <- st_sf(western_flat_subset)
st_crs(western_flat_sf) <- 4326
set.seed(123)
western_flat_sample <- western_flat_sf[sample(nrow(western_flat_sf), size = 10000), ]
western_flat_sample <- st_cast(western_flat_sample, "POINT")
coords <- st_coordinates(western_flat_sample)
coords_jittered <- coords + matrix(rnorm(n = length(coords), mean = 0, sd = 1e-4), ncol = 2)
western_flat_sample$geometry <- st_sfc(lapply(1:nrow(coords_jittered), function(i) {
  st_point(coords_jittered[i, ])
}), crs = st_crs(western_flat_sample))

western_flat_sampled_model <- lm(log_price_in_2010_pounds ~ 
                                   close_to_crossrail_network * post_treatment +  
                                   log_population_density + 
                                   avg_bedrooms + 
                                   average_qualification_level_4 + 
                                    
                                   employment_pct_60 + 
                                   old_new +
                                   log_subcentre_distance +
                                   # travel_time_p50 +
                                   noiseclass +
                                   log_crime_count +
                                   access_parks_30_pct,
                                 data = western_flat_sample)

residuals <- residuals(western_flat_sampled_model)
nb <- knn2nb(knearneigh(coords_jittered, k = 8))
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)
moran_test <- moran.test(residuals, lw)
print(moran_test)

western_flat_slm <- lagsarlm(log_price_in_2010_pounds ~ 
                               close_to_crossrail_network * post_treatment +  
                               log_population_density + 
                               avg_bedrooms + 
                               average_qualification_level_4 + 
                                
                               employment_pct_60 + 
                               old_new +
                               log_subcentre_distance +
                               # travel_time_p50 +
                               noiseclass +
                               log_crime_count +
                               access_parks_30_pct,
                             data = western_flat_sample, listw = lw)

save(western_flat_slm, file = "western_flat_slm.RData")

#  random slope model: allowing predictors to have varying slopes across LSOAs
western_flat_sample_model <- lmer(log_price_in_2010_pounds ~ 
                            close_to_crossrail_network * post_treatment +  
                            log_population_density + 
                            avg_bedrooms + 
                            average_qualification_level_4 + 
                            employment_pct_60 + 
                            old_new +
                            log_subcentre_distance +
                            # travel_time_p50 +
                            noiseclass +
                            log_crime_count +
                            access_parks_30_pct +
                            (1  + post_treatment | LSOA11CD) ,
                          data = western_flat_sample)
# Summary of the model
summary(western_flat_sample_model)

# EASTERN FLAT MODEL
eastern_flat_sf <- st_sf(eastern_flat_subset)
st_crs(eastern_flat_sf) <- 4326
set.seed(123)
eastern_flat_sample <- eastern_flat_sf[sample(nrow(eastern_flat_sf), size = 10000), ]
eastern_flat_sample <- st_cast(eastern_flat_sample, "POINT")
coords <- st_coordinates(eastern_flat_sample)
coords_jittered <- coords + matrix(rnorm(n = length(coords), mean = 0, sd = 1e-4), ncol = 2)
eastern_flat_sample$geometry <- st_sfc(lapply(1:nrow(coords_jittered), function(i) {
  st_point(coords_jittered[i, ])
}), crs = st_crs(eastern_flat_sample))

eastern_flat_sampled_model <- lm(log_price_in_2010_pounds ~ 
                                   close_to_crossrail_network * post_treatment +  
                                   log_population_density + 
                                   avg_bedrooms + 
                                   average_qualification_level_4 + 
                                    
                                   employment_pct_60 + 
                                   old_new +
                                   log_subcentre_distance +
                                   # travel_time_p50 +
                                   noiseclass +
                                   log_crime_count +
                                   access_parks_30_pct,
                                 data = eastern_flat_sample)

residuals <- residuals(eastern_flat_sampled_model)
nb <- knn2nb(knearneigh(coords_jittered, k = 8))
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)
moran_test <- moran.test(residuals, lw)
print(moran_test)

eastern_flat_slm <- lagsarlm(log_price_in_2010_pounds ~ 
                               close_to_crossrail_network * post_treatment +  
                               log_population_density + 
                               avg_bedrooms + 
                               average_qualification_level_4 + 
                                
                               employment_pct_60 + 
                               old_new +
                               log_subcentre_distance +
                               # travel_time_p50 +
                               noiseclass +
                               log_crime_count +
                               access_parks_30_pct,
                             data = eastern_flat_sample, listw = lw)

save(eastern_flat_slm, file = "eastern_flat_slm.RData")

#  random slope model: allowing predictors to have varying slopes across LSOAs
eastern_flat_sample_model <- lmer(log_price_in_2010_pounds ~ 
                            close_to_crossrail_network * post_treatment +  
                            log_population_density + 
                            avg_bedrooms + 
                            average_qualification_level_4 + 
                            employment_pct_60 + 
                            old_new +
                            log_subcentre_distance +
                            # travel_time_p50 +
                            noiseclass +
                            log_crime_count +
                            access_parks_30_pct +
                            (1  + post_treatment | LSOA11CD) ,
                          data = eastern_flat_sample)
# Summary of the model
summary(eastern_flat_sample_model)

# WESTERN SEMI-DETACHED MODEL
western_semi_detached_sf <- st_sf(western_semi_detached_subset)
st_crs(western_semi_detached_sf) <- 4326
set.seed(123)
western_semi_detached_sample <- western_semi_detached_sf[sample(nrow(western_semi_detached_sf), size = 10000), ]
western_semi_detached_sample <- st_cast(western_semi_detached_sample, "POINT")
coords <- st_coordinates(western_semi_detached_sample)
coords_jittered <- coords + matrix(rnorm(n = length(coords), mean = 0, sd = 1e-4), ncol = 2)
western_semi_detached_sample$geometry <- st_sfc(lapply(1:nrow(coords_jittered), function(i) {
  st_point(coords_jittered[i, ])
}), crs = st_crs(western_semi_detached_sample))

western_semi_detached_sampled_model <- lm(log_price_in_2010_pounds ~ 
                                            close_to_crossrail_network * post_treatment +  
                                            log_population_density + 
                                            avg_bedrooms + 
                                            average_qualification_level_4 + 
                                             
                                            employment_pct_60 + 
                                            old_new +
                                            log_subcentre_distance +
                                            # travel_time_p50 +
                                            noiseclass +
                                            log_crime_count +
                                            access_parks_30_pct,
                                          data = western_semi_detached_sample)

residuals <- residuals(western_semi_detached_sampled_model)
nb <- knn2nb(knearneigh(coords_jittered, k = 8))
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)
moran_test <- moran.test(residuals, lw)
print(moran_test)

western_semi_detached_slm <- lagsarlm(log_price_in_2010_pounds ~ 
                                        close_to_crossrail_network * post_treatment +  
                                        log_population_density + 
                                        avg_bedrooms + 
                                        average_qualification_level_4 + 
                                         
                                        employment_pct_60 + 
                                        old_new +
                                        log_subcentre_distance +
                                        # travel_time_p50 +
                                        noiseclass +
                                        log_crime_count +
                                        access_parks_30_pct,
                                      data = western_semi_detached_sample, listw = lw)

save(western_semi_detached_slm, file = "western_semi_detached_slm.RData")

#  random slope model: allowing predictors to have varying slopes across LSOAs
western_semi_detached_sample_model <- lmer(log_price_in_2010_pounds ~ 
                            close_to_crossrail_network * post_treatment +  
                            log_population_density + 
                            avg_bedrooms + 
                            average_qualification_level_4 + 
                            employment_pct_60 + 
                            old_new +
                            log_subcentre_distance +
                            # travel_time_p50 +
                            noiseclass +
                            log_crime_count +
                            access_parks_30_pct +
                            (1  + post_treatment | LSOA11CD) ,
                          data = western_semi_detached_sample)
# Summary of the model
summary(western_semi_detached_sample_model)

# EASTERN SEMI-DETACHED MODEL
eastern_semi_detached_sf <- st_sf(eastern_semi_detached_subset)
st_crs(eastern_semi_detached_sf) <- 4326
set.seed(123)
eastern_semi_detached_sample <- eastern_semi_detached_sf[sample(nrow(eastern_semi_detached_sf), size = 10000), ]
eastern_semi_detached_sample <- st_cast(eastern_semi_detached_sample, "POINT")
coords <- st_coordinates(eastern_semi_detached_sample)
coords_jittered <- coords + matrix(rnorm(n = length(coords), mean = 0, sd = 1e-4), ncol = 2)
eastern_semi_detached_sample$geometry <- st_sfc(lapply(1:nrow(coords_jittered), function(i) {
  st_point(coords_jittered[i, ])
}), crs = st_crs(eastern_semi_detached_sample))

eastern_semi_detached_sampled_model <- lm(log_price_in_2010_pounds ~ 
                                            close_to_crossrail_network * post_treatment +  
                                            log_population_density + 
                                            avg_bedrooms + 
                                            average_qualification_level_4 + 
                                             
                                            employment_pct_60 + 
                                            old_new +
                                            log_subcentre_distance +
                                            # travel_time_p50 +
                                            noiseclass +
                                            log_crime_count +
                                            access_parks_30_pct,
                                          data = eastern_semi_detached_sample)

residuals <- residuals(eastern_semi_detached_sampled_model)
nb <- knn2nb(knearneigh(coords_jittered, k = 8))
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)
moran_test <- moran.test(residuals, lw)
print(moran_test)

eastern_semi_detached_slm <- lagsarlm(log_price_in_2010_pounds ~ 
                                        close_to_crossrail_network * post_treatment +  
                                        log_population_density + 
                                        avg_bedrooms + 
                                        average_qualification_level_4 + 
                                         
                                        employment_pct_60 + 
                                        old_new +
                                        log_subcentre_distance +
                                        # travel_time_p50 +
                                        noiseclass +
                                        log_crime_count +
                                        access_parks_30_pct,
                                      data = eastern_semi_detached_sample, listw = lw)

save(eastern_semi_detached_slm, file = "eastern_semi_detached_slm.RData")

#  random slope model: allowing predictors to have varying slopes across LSOAs
eastern_semi_detached_sample_model <- lmer(log_price_in_2010_pounds ~ 
                            close_to_crossrail_network * post_treatment +  
                            log_population_density + 
                            avg_bedrooms + 
                            average_qualification_level_4 + 
                            employment_pct_60 + 
                            old_new +
                            log_subcentre_distance +
                            # travel_time_p50 +
                            noiseclass +
                            log_crime_count +
                            access_parks_30_pct +
                            (1  + post_treatment | LSOA11CD) ,
                          data = eastern_semi_detached_sample)
# Summary of the model
summary(eastern_semi_detached_sample_model)

# WESTERN TERRACED MODEL
western_terraced_sf <- st_sf(western_terraced_subset)
st_crs(western_terraced_sf) <- 4326
set.seed(123)
western_terraced_sample <- western_terraced_sf[sample(nrow(western_terraced_sf), size = 10000), ]
western_terraced_sample <- st_cast(western_terraced_sample, "POINT")
coords <- st_coordinates(western_terraced_sample)
coords_jittered <- coords + matrix(rnorm(n = length(coords), mean = 0, sd = 1e-4), ncol = 2)
western_terraced_sample$geometry <- st_sfc(lapply(1:nrow(coords_jittered), function(i) {
  st_point(coords_jittered[i, ])
}), crs = st_crs(western_terraced_sample))

western_terraced_sampled_model <- lm(log_price_in_2010_pounds ~ 
                                       close_to_crossrail_network * post_treatment +  
                                       log_population_density + 
                                       avg_bedrooms + 
                                       average_qualification_level_4 + 
                                        
                                       employment_pct_60 + 
                                       old_new +
                                       log_subcentre_distance +
                                       # travel_time_p50 +
                                       noiseclass +
                                       log_crime_count +
                                       access_parks_30_pct,
                                     data = western_terraced_sample)

residuals <- residuals(western_terraced_sampled_model)
nb <- knn2nb(knearneigh(coords_jittered, k = 8))
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)
moran_test <- moran.test(residuals, lw)
print(moran_test)

western_terraced_slm <- lagsarlm(log_price_in_2010_pounds ~ 
                                   close_to_crossrail_network * post_treatment +  
                                   log_population_density + 
                                   avg_bedrooms + 
                                   average_qualification_level_4 + 
                                    
                                   employment_pct_60 + 
                                   old_new +
                                   log_subcentre_distance +
                                   # travel_time_p50 +
                                   noiseclass +
                                   log_crime_count +
                                   access_parks_30_pct,
                                 data = western_terraced_sample, listw = lw)

save(western_terraced_slm, file = "western_terraced_slm.RData")

#  random slope model: allowing predictors to have varying slopes across LSOAs
western_terraced_sample_model <- lmer(log_price_in_2010_pounds ~ 
                            close_to_crossrail_network * post_treatment +  
                            log_population_density + 
                            avg_bedrooms + 
                            average_qualification_level_4 + 
                            employment_pct_60 + 
                            old_new +
                            log_subcentre_distance +
                            # travel_time_p50 +
                            noiseclass +
                            log_crime_count +
                            access_parks_30_pct +
                            (1  + post_treatment | LSOA11CD) ,
                          data = western_terraced_sample)
# Summary of the model
summary(western_terraced_sample_model)

# EASTERN TERRACED MODEL
eastern_terraced_sf <- st_sf(eastern_terraced_subset)
st_crs(eastern_terraced_sf) <- 4326
set.seed(123)
eastern_terraced_sample <- eastern_terraced_sf[sample(nrow(eastern_terraced_sf), size = 10000), ]
eastern_terraced_sample <- st_cast(eastern_terraced_sample, "POINT")
coords <- st_coordinates(eastern_terraced_sample)
coords_jittered <- coords + matrix(rnorm(n = length(coords), mean = 0, sd = 1e-4), ncol = 2)
eastern_terraced_sample$geometry <- st_sfc(lapply(1:nrow(coords_jittered), function(i) {
  st_point(coords_jittered[i, ])
}), crs = st_crs(eastern_terraced_sample))

eastern_terraced_sampled_model <- lm(log_price_in_2010_pounds ~ 
                                       close_to_crossrail_network * post_treatment +  
                                       log_population_density + 
                                       avg_bedrooms + 
                                       average_qualification_level_4 + 
                                        
                                       employment_pct_60 + 
                                       old_new +
                                       log_subcentre_distance +
                                       # travel_time_p50 +
                                       noiseclass +
                                       log_crime_count +
                                       access_parks_30_pct,
                                     data = eastern_terraced_sample)

residuals <- residuals(eastern_terraced_sampled_model)
nb <- knn2nb(knearneigh(coords_jittered, k = 8))
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)
moran_test <- moran.test(residuals, lw)
print(moran_test)

eastern_terraced_slm <- lagsarlm(log_price_in_2010_pounds ~ 
                                   close_to_crossrail_network * post_treatment +  
                                   log_population_density + 
                                   avg_bedrooms + 
                                   average_qualification_level_4 + 
                                    
                                   employment_pct_60 + 
                                   old_new +
                                   log_subcentre_distance +
                                   # travel_time_p50 +
                                   noiseclass +
                                   log_crime_count +
                                   access_parks_30_pct,
                                 data = eastern_terraced_sample, listw = lw)

save(eastern_terraced_slm, file = "eastern_terraced_slm.RData")

#  random slope model: allowing predictors to have varying slopes across LSOAs
eastern_terraced_sample_model <- lmer(log_price_in_2010_pounds ~ 
                            close_to_crossrail_network * post_treatment +  
                            log_population_density + 
                            avg_bedrooms + 
                            average_qualification_level_4 + 
                            employment_pct_60 + 
                            old_new +
                            log_subcentre_distance +
                            # travel_time_p50 +
                            noiseclass +
                            log_crime_count +
                            access_parks_30_pct +
                            (1  + post_treatment | LSOA11CD) ,
                          data = eastern_terraced_sample)
# Summary of the model
summary(eastern_terraced_sample_model)

# CENTRAL FLAT MODEL
central_flat_sf <- st_sf(central_flat_subset)
st_crs(central_flat_sf) <- 4326
set.seed(123)
central_flat_sample <- central_flat_sf[sample(nrow(central_flat_sf), size = 10000), ]
central_flat_sample <- st_cast(central_flat_sample, "POINT")
coords <- st_coordinates(central_flat_sample)
coords_jittered <- coords + matrix(rnorm(n = length(coords), mean = 0, sd = 1e-4), ncol = 2)
central_flat_sample$geometry <- st_sfc(lapply(1:nrow(coords_jittered), function(i) {
  st_point(coords_jittered[i, ])
}), crs = st_crs(central_flat_sample))

central_flat_sampled_model <- lm(log_price_in_2010_pounds ~ 
                                   close_to_crossrail_network * post_treatment +  
                                   log_population_density + 
                                   avg_bedrooms + 
                                   average_qualification_level_4 + 
                                    
                                   employment_pct_60 + 
                                   old_new +
                                   log_subcentre_distance +
                                   # travel_time_p50 +
                                   noiseclass +
                                   log_crime_count +
                                   access_parks_30_pct,
                                 data = central_flat_sample)

residuals <- residuals(central_flat_sampled_model)
nb <- knn2nb(knearneigh(coords_jittered, k = 8))
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)
moran_test <- moran.test(residuals, lw)
print(moran_test)

central_flat_slm <- lagsarlm(log_price_in_2010_pounds ~ 
                               close_to_crossrail_network * post_treatment +  
                               log_population_density + 
                               avg_bedrooms + 
                               average_qualification_level_4 + 
                                
                               employment_pct_60 + 
                               old_new +
                               log_subcentre_distance +
                               # travel_time_p50 +
                               noiseclass +
                               log_crime_count +
                               access_parks_30_pct,
                             data = central_flat_sample, listw = lw)

save(central_flat_slm, file = "central_flat_slm.RData")

#  random slope model: allowing predictors to have varying slopes across LSOAs
central_flat_sample_model <- lmer(log_price_in_2010_pounds ~ 
                            close_to_crossrail_network * post_treatment +  
                            log_population_density + 
                            avg_bedrooms + 
                            average_qualification_level_4 + 
                            employment_pct_60 + 
                            old_new +
                            log_subcentre_distance +
                            # travel_time_p50 +
                            noiseclass +
                            log_crime_count +
                            access_parks_30_pct +
                            (1  + post_treatment | LSOA11CD) ,
                          data = central_flat_sample)
# Summary of the model
summary(central_flat_sample_model)

# CENTRAL SEMI-DETACHED MODEL
central_semi_detached_sf <- st_sf(central_semi_detached_subset)
st_crs(central_semi_detached_sf) <- 4326
set.seed(123)
central_semi_detached_sample <- central_semi_detached_sf
central_semi_detached_sample <- st_cast(central_semi_detached_sample, "POINT")
coords <- st_coordinates(central_semi_detached_sample)
coords_jittered <- coords + matrix(rnorm(n = length(coords), mean = 0, sd = 1e-4), ncol = 2)
central_semi_detached_sample$geometry <- st_sfc(lapply(1:nrow(coords_jittered), function(i) {
  st_point(coords_jittered[i, ])
}), crs = st_crs(central_semi_detached_sample))

central_semi_detached_sampled_model <- lm(log_price_in_2010_pounds ~ 
                                            close_to_crossrail_network * post_treatment +  
                                            log_population_density + 
                                            avg_bedrooms + 
                                            average_qualification_level_4 + 
                                            
                                            employment_pct_60 + 
                                            old_new +
                                            log_subcentre_distance +
                                            # travel_time_p50 +
                                            noiseclass +
                                            log_crime_count +
                                            access_parks_30_pct,
                                          data = central_semi_detached_sample)

residuals <- residuals(central_semi_detached_sampled_model)
nb <- knn2nb(knearneigh(coords_jittered, k = 8))
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)
moran_test <- moran.test(residuals, lw)
print(moran_test)

central_semi_detached_slm <- lagsarlm(log_price_in_2010_pounds ~ 
                                        close_to_crossrail_network * post_treatment +  
                                        log_population_density + 
                                        avg_bedrooms + 
                                        average_qualification_level_4 + 
                                        
                                        employment_pct_60 + 
                                        old_new +
                                        log_subcentre_distance +
                                        # travel_time_p50 +
                                        noiseclass +
                                        log_crime_count +
                                        access_parks_30_pct, listw = lw)

save(central_semi_detached_slm, file = "central_semi_detached_slm.RData")

#  random slope model: allowing predictors to have varying slopes across LSOAs
central_semi_detached_sample_model <- lmer(log_price_in_2010_pounds ~ 
                            close_to_crossrail_network * post_treatment +  
                            log_population_density + 
                            avg_bedrooms + 
                            average_qualification_level_4 + 
                            employment_pct_60 + 
                            old_new +
                            log_subcentre_distance +
                            # travel_time_p50 +
                            noiseclass +
                            log_crime_count +
                            access_parks_30_pct +
                            (1  + post_treatment | LSOA11CD) ,
                          data = central_semi_detached_sample)
# Summary of the model
summary(central_semi_detached_sample_model)

# CENTRAL TERRACED MODEL
central_terraced_sf <- st_sf(central_terraced_subset)
st_crs(central_terraced_sf) <- 4326
set.seed(123)
central_terraced_sample <- central_terraced_sf
central_terraced_sample <- st_cast(central_terraced_sample, "POINT")
coords <- st_coordinates(central_terraced_sample)
coords_jittered <- coords + matrix(rnorm(n = length(coords), mean = 0, sd = 1e-4), ncol = 2)
central_terraced_sample$geometry <- st_sfc(lapply(1:nrow(coords_jittered), function(i) {
  st_point(coords_jittered[i, ])
}), crs = st_crs(central_terraced_sample))

central_terraced_sampled_model <- lm(log_price_in_2010_pounds ~ 
                                       close_to_crossrail_network * post_treatment +  
                                       log_population_density + 
                                       avg_bedrooms + 
                                       average_qualification_level_4 + 
                                        
                                       employment_pct_60 + 
                                       old_new +
                                       log_subcentre_distance +
                                       # travel_time_p50 +
                                       noiseclass +
                                       log_crime_count +
                                       access_parks_30_pct,
                                     data = central_terraced_sample)

residuals <- residuals(central_terraced_sampled_model)
nb <- knn2nb(knearneigh(coords_jittered, k = 8))
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)
moran_test <- moran.test(residuals, lw)
print(moran_test)

central_terraced_slm <- lagsarlm(log_price_in_2010_pounds ~ 
                                   close_to_crossrail_network * post_treatment +  
                                   log_population_density + 
                                   avg_bedrooms + 
                                   average_qualification_level_4 + 
                                   employment_pct_60 + 
                                   old_new +
                                   log_subcentre_distance +
                                   # travel_time_p50 +
                                   noiseclass +
                                   log_crime_count +
                                   access_parks_30_pct,
                                 data = central_terraced_sample, listw = lw)

save(central_terraced_slm, file = "central_terraced_slm.RData")

#  random slope model: allowing predictors to have varying slopes across LSOAs
central_terraced_sample_model <- lmer(log_price_in_2010_pounds ~ 
                            close_to_crossrail_network * post_treatment +  
                            log_population_density + 
                            avg_bedrooms + 
                            average_qualification_level_4 + 
                            employment_pct_60 + 
                            old_new +
                            log_subcentre_distance +
                            # travel_time_p50 +
                            noiseclass +
                            log_crime_count +
                            access_parks_30_pct +
                            (1  + post_treatment | LSOA11CD) ,
                          data = central_terraced_sample)
# Summary of the model
summary(central_terraced_sample_model)

# CENTRAL DETACHED MODEL
central_detached_sf <- st_sf(central_detached_subset)
st_crs(central_detached_sf) <- 4326
set.seed(123)
central_detached_sample <- central_detached_sf
central_detached_sample <- st_cast(central_detached_sample, "POINT")
coords <- st_coordinates(central_detached_sample)
coords_jittered <- coords + matrix(rnorm(n = length(coords), mean = 0, sd = 1e-4), ncol = 2)
central_detached_sample$geometry <- st_sfc(lapply(1:nrow(coords_jittered), function(i) {
  st_point(coords_jittered[i, ])
}), crs = st_crs(central_detached_sample))

central_detached_sampled_model <- lm(log_price_in_2010_pounds ~ 
                                       close_to_crossrail_network * post_treatment +  
                                       log_population_density + 
                                       avg_bedrooms + 
                                       average_qualification_level_4 + 
                                       
                                       employment_pct_60 + 
                                       old_new +
                                       log_subcentre_distance +
                                       # travel_time_p50 +
                                       noiseclass +
                                       log_crime_count +
                                       access_parks_30_pct,
                                     data = central_detached_sample)

residuals <- residuals(central_detached_sampled_model)
nb <- knn2nb(knearneigh(coords_jittered, k = 8))
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)
moran_test <- moran.test(residuals, lw)
print(moran_test)

central_detached_slm <- lagsarlm(log_price_in_2010_pounds ~ 
                                   close_to_crossrail_network * post_treatment +  
                                   log_population_density + 
                                   avg_bedrooms + 
                                   average_qualification_level_4 + 
                                   
                                   employment_pct_60 + 
                                   old_new +
                                   log_subcentre_distance +
                                   # travel_time_p50 +
                                   noiseclass +
                                   log_crime_count +
                                   access_parks_30_pct,
                                 data = central_detached_sample, listw = lw)
summary(central_detached_slm)

save(central_detached_slm, file = "central_detached_slm.RData")

#  random slope model: allowing predictors to have varying slopes across LSOAs
central_detached_sample_model <- lmer(log_price_in_2010_pounds ~ 
                            close_to_crossrail_network * post_treatment +  
                            log_population_density + 
                            avg_bedrooms + 
                            average_qualification_level_4 + 
                            employment_pct_60 + 
                            old_new +
                            log_subcentre_distance +
                            # travel_time_p50 +
                            noiseclass +
                            log_crime_count +
                            access_parks_30_pct +
                            (1  + post_treatment | LSOA11CD) ,
                          data = central_detached_sample)
# Summary of the model
summary(central_detached_sample_model)

