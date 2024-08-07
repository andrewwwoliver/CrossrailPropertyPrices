# Propensity score matching model


# Install and load necessary packages
library(MatchIt)
library(dplyr)
library(cobalt)
library(ggplot2)

# Step 1: Estimate propensity scores and perform matching
# Assuming 'close_to_crossrail' is the treatment indicator
matchit_formula <- close_to_crossrail ~ post_treatment +
  log_underground_distance  +  log_population_density  + 
  log_min_distance_to_caz + semi_detached_binary + flat_binary + 
  avg_bedrooms + average_qualification_level_4

# remove D from each of the datasets
non_centred_properties_with_census 
non_centred_western_subset 
non_centred_eastern_subset 
non_centred_central_subset 

terraced_non_centred_properties_with_census <- non_centred_properties_with_census %>%
  filter(property_type == "T")
terraced_non_centred_western_subset <- non_centred_western_subset %>%
  filter(property_type == "T")
terraced_non_centred_eastern_subset <- non_centred_eastern_subset %>%
  filter(property_type == "T")
terraced_non_centred_central_subset <- non_centred_central_subset %>%
  filter(property_type == "T")

semi_detached_non_centred_properties_with_census <- non_centred_properties_with_census %>%
  filter(property_type == "S")
semi_detached_non_centred_western_subset <- non_centred_western_subset %>%
  filter(property_type == "S")
semi_detached_non_centred_eastern_subset <- non_centred_eastern_subset %>%
  filter(property_type == "S")
semi_detached_non_centred_central_subset <- non_centred_central_subset %>%
  filter(property_type == "S")

flat_non_centred_properties_with_census <- non_centred_properties_with_census %>%
  filter(property_type == "F")
flat_non_centred_western_subset <- non_centred_western_subset %>%
  filter(property_type == "F")
flat_non_centred_eastern_subset <- non_centred_eastern_subset %>%
  filter(property_type == "F")
flat_non_centred_central_subset <- non_centred_central_subset %>%
  filter(property_type == "F")

detached_non_centred_properties_with_census <- non_centred_properties_with_census %>%
  filter(property_type == "D")
detached_non_centred_western_subset <- non_centred_western_subset %>%
  filter(property_type == "D")
detached_non_centred_eastern_subset <- non_centred_eastern_subset %>%
  filter(property_type == "D")
detached_non_centred_central_subset <- non_centred_central_subset %>%
  filter(property_type == "D")




#perform nearest neighbour matching
terraced_full_psm <- matchit(matchit_formula, data = terraced_non_centred_properties_with_census, method = "nearest")
terraced_western_psm <- matchit(matchit_formula, data = terraced_non_centred_western_subset, method = "nearest")
terraced_eastern_psm <- matchit(matchit_formula, data = terraced_non_centred_eastern_subset, method = "nearest")
terraced_central_psm <- matchit(matchit_formula, data = terraced_non_centred_central_subset, method = "nearest")

semi_detached_full_psm <- matchit(matchit_formula, data = semi_detached_non_centred_properties_with_census, method = "nearest")
semi_detached_western_psm <- matchit(matchit_formula, data = semi_detached_non_centred_western_subset, method = "nearest")
semi_detached_eastern_psm <- matchit(matchit_formula, data = semi_detached_non_centred_eastern_subset, method = "nearest")
semi_detached_central_psm <- matchit(matchit_formula, data = semi_detached_non_centred_central_subset, method = "nearest")

flat_full_psm <- matchit(matchit_formula, data = flat_non_centred_properties_with_census, method = "nearest")
flat_western_psm <- matchit(matchit_formula, data = flat_non_centred_western_subset, method = "nearest")
flat_eastern_psm <- matchit(matchit_formula, data = flat_non_centred_eastern_subset, method = "nearest")
flat_central_psm <- matchit(matchit_formula, data = flat_non_centred_central_subset, method = "nearest")

detached_full_psm <- matchit(matchit_formula, data = detached_non_centred_properties_with_census, method = "nearest")
detached_western_psm <- matchit(matchit_formula, data = detached_non_centred_western_subset, method = "nearest")
detached_eastern_psm <- matchit(matchit_formula, data = detached_non_centred_eastern_subset, method = "nearest")
detached_central_psm <- matchit(matchit_formula, data = detached_non_centred_central_subset, method = "nearest")




#set.cobalt.options(binary = "std")

# variable names 
new.names <- c(flat_binary = "Flat",
               terraced_binary = "Terraced",
               close_to_crossrail = "Close to Crossrail",
               log_underground_distance = "Underground Distance",
               log_population_density = "Population Density",
               employment_rate = "Employment Rate",
               percent_households_deprived = "Household Deprivation")

# Step 2: Check the balance of covariates
# copy back in for graphs with relevant data

#full_plot
#western_plot
#eastern_plot
#central_plot

#extract matched data
detached_full_matched_data <- match.data(detached_full_psm)
detached_western_matched_data <- match.data(detached_western_psm)
detached_eastern_matched_data <- match.data(detached_eastern_psm)
detached_central_matched_data <- match.data(detached_central_psm)

semi_detached_full_matched_data <- match.data(semi_detached_full_psm)
semi_detached_western_matched_data <- match.data(semi_detached_western_psm)
semi_detached_eastern_matched_data <- match.data(semi_detached_eastern_psm)
semi_detached_central_matched_data <- match.data(semi_detached_central_psm)

flat_full_matched_data <- match.data(flat_full_psm)
flat_western_matched_data <- match.data(flat_western_psm)
flat_eastern_matched_data <- match.data(flat_eastern_psm)
flat_central_matched_data <- match.data(flat_central_psm)

terraced_full_matched_data <- match.data(terraced_full_psm)
terraced_western_matched_data <- match.data(terraced_western_psm)
terraced_eastern_matched_data <- match.data(terraced_eastern_psm)
terraced_central_matched_data <- match.data(terraced_central_psm)

# Step 4: Run the DiD regression model on matched data
detached_full_model <- lm(log_price ~ close_to_crossrail * post_treatment + 
                            log_underground_distance + log_population_density +
                            log_min_distance_to_caz + avg_bedrooms + 
                            average_qualification_level_4, 
                          data = subset(properties_with_census, property_type == "D"))

detached_western_model <- lm(log_price ~ close_to_crossrail * post_treatment + 
                              log_underground_distance + log_population_density +
                              log_min_distance_to_caz + avg_bedrooms + 
                              average_qualification_level_4, 
                            data = subset(western_subset, property_type == "D"))

detached_eastern_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                              log_underground_distance + log_population_density +
                              log_min_distance_to_caz + avg_bedrooms + 
                              average_qualification_level_4, 
                             data = subset(eastern_subset, property_type == "D"))

detached_central_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                              log_underground_distance + log_population_density +
                              log_min_distance_to_caz + avg_bedrooms + 
                              average_qualification_level_4, 
                             data = subset(central_subset, property_type == "D"))

semi_detached_full_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                                log_underground_distance + log_population_density +
                                log_min_distance_to_caz + avg_bedrooms + 
                                average_qualification_level_4, 
                               data = subset(properties_with_census, property_type == "S"))

semi_detached_western_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                                  log_underground_distance + log_population_density +
                                  log_min_distance_to_caz + avg_bedrooms + 
                                  average_qualification_level_4, 
                                  data = subset(western_subset, property_type == "S"))

semi_detached_eastern_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                                  log_underground_distance + log_population_density +
                                  log_min_distance_to_caz + avg_bedrooms + 
                                  average_qualification_level_4, 
                                  data = subset(eastern_subset, property_type == "S"))

semi_detached_central_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                                  log_underground_distance + log_population_density +
                                  log_min_distance_to_caz + avg_bedrooms + 
                                  average_qualification_level_4,
                                  data = semi_detached_central_matched_data)

flat_full_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                      log_underground_distance + log_population_density +
                      log_min_distance_to_caz + avg_bedrooms + 
                      average_qualification_level_4, data = flat_full_matched_data)

flat_western_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                        log_underground_distance + log_population_density +
                        log_min_distance_to_caz + avg_bedrooms + 
                        average_qualification_level_4, data = flat_western_matched_data)

flat_eastern_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                        log_underground_distance + log_population_density +
                        log_min_distance_to_caz + avg_bedrooms + 
                        average_qualification_level_4, data = flat_eastern_matched_data)

flat_central_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                        log_underground_distance + log_population_density +
                        log_min_distance_to_caz + avg_bedrooms + 
                        average_qualification_level_4, data = flat_central_matched_data)

terraced_full_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                          log_underground_distance + log_population_density +
                          log_min_distance_to_caz + avg_bedrooms + 
                          average_qualification_level_4, data = terraced_full_matched_data)

terraced_western_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                            log_underground_distance + log_population_density +
                            log_min_distance_to_caz + avg_bedrooms + 
                            average_qualification_level_4, data = terraced_western_matched_data)

terraced_eastern_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                            log_underground_distance + log_population_density +
                            log_min_distance_to_caz + avg_bedrooms + 
                            average_qualification_level_4, data = terraced_eastern_matched_data)

terraced_central_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                            log_underground_distance + log_population_density +
                            log_min_distance_to_caz + avg_bedrooms + 
                            average_qualification_level_4, data = terraced_central_matched_data)

#star gazer for each model
 stargazer(detached_full_model, detached_western_model,
           type = "html", summary = FALSE, rownames = FALSE,
           title = "Difference-in-Differences Regression Models",
           column.labels = c("Full", "Western", "Eastern", "Central"),
           dep.var.labels = "Log Price",
           out = "detached_psm_models.htm")
 
 #stargazer for  detached_eastern_model, detached_central_model
 stargazer(detached_eastern_model, detached_central_model,
           type = "html", summary = FALSE, rownames = FALSE,
           title = "Difference-in-Differences Regression Models",
           column.labels = c("Eastern", "Central"),
           dep.var.labels = "Log Price",
           out = "detached_psm_models2.htm")
 
 #stargazer for semi_detached_full_model, semi_detached_western_model
 stargazer(semi_detached_full_model, semi_detached_western_model,
           type = "html", summary = FALSE, rownames = FALSE,
           title = "Difference-in-Differences Regression Models",
           column.labels = c("Full", "Western"),
           dep.var.labels = "Log Price",
           out = "semi_detached_psm_models.htm")
 
 #stargazer for semi_detached_eastern_model, semi_detached_central_model
 stargazer(semi_detached_eastern_model, semi_detached_central_model,
           type = "html", summary = FALSE, rownames = FALSE,
           title = "Difference-in-Differences Regression Models",
           column.labels = c("Eastern", "Central"),
           dep.var.labels = "Log Price",
           out = "semi_detached_psm_models2.htm")
 
 #stargazer for flat_full_model, flat_western_model
 stargazer(flat_full_model, flat_western_model,
           type = "html", summary = FALSE, rownames = FALSE,
           title = "Difference-in-Differences Regression Models",
           column.labels = c("Full", "Western"),
           dep.var.labels = "Log Price",
           out = "flat_psm_models.htm")
 
 #stargazer for flat_eastern_model, flat_central_model
 stargazer(flat_eastern_model, flat_central_model,
           type = "html", summary = FALSE, rownames = FALSE,
           title = "Difference-in-Differences Regression Models",
           column.labels = c("Eastern", "Central"),
           dep.var.labels = "Log Price",
           out = "flat_psm_models2.htm")
 
 #stargazer for terraced_full_model, terraced_western_model
 stargazer(terraced_full_model, terraced_western_model,
           type = "html", summary = FALSE, rownames = FALSE,
           title = "Difference-in-Differences Regression Models",
           column.labels = c("Full", "Western"),
           dep.var.labels = "Log Price",
           out = "terraced_psm_models.htm")
 
 #stargazer for terraced_eastern_model, terraced_central_model
 stargazer(terraced_eastern_model, terraced_central_model,
           type = "html", summary = FALSE, rownames = FALSE,
           title = "Difference-in-Differences Regression Models",
           column.labels = c("Eastern", "Central"),
           dep.var.labels = "Log Price",
           out = "terraced_psm_models2.htm")
 
 
 


# Step 5: Calculate the average treatment effect on the treated (ATT)
# Calculate the ATT for each model
detached_full_att <- att(detached_full_model, data = detached_full_matched_data, method = "regression")
