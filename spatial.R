
library(Matrix)
library(spatialreg)
library(spdep)
library(sf)
library(sp)
# Convert the data.frame to sf object
western_subset <- st_sf(western_subset)

# Ensure CRS is set (assuming WGS 84 CRS)
st_crs(western_subset) <- 4326


# Extract coordinates
coords <- st_coordinates(western_subset)

# Fit the Spatial Lag Model using sparse matrices
sampled_model <- lm(log_price ~ close_to_crossrail + post_treatment +
                      log_underground_distance  +  log_population_density  + 
                      log_min_distance_to_caz  + flat_binary + semi_detached_binary +
                      avg_bedrooms + average_qualification_level_4,
                    data = western_subset)
#Jitter the coordinates to avoid identical points
jitter_amount <- 1e-4
coords_jittered <- coords + matrix(rnorm(n = length(coords), mean = 0, sd = jitter_amount), ncol = 2)

# Update geometry with jittered coordinates
western_subset$geometry <- st_sfc(st_multipoint(coords_jittered), crs = 4326)
# Extract jittered coordinates
coords_jittered <- st_coordinates(western_subset)

# Calculate Moran's I on the residuals
residuals <- residuals(sampled_model)
nb <- knn2nb(knearneigh(coords_jittered, k = 8))  # k nearest neighbors
lw <- nb2listw(nb, style = "W", zero.policy=TRUE)
moran_test <- moran.test(residuals, lw)


print(moran_test)

# Fit a Spatial Lag Model
slm_model <- lagsarlm(log_price ~ close_to_crossrail * post_treatment +
                        log_underground_distance + log_population_density + 
                        employment_rate + percent_households_deprived + 
                        terraced_binary + flat_binary,
                      data = sampled_data, listw = lw)

# Summarize the Spatial Lag Model
summary(slm_model)


sem_model <- errorsarlm(log_price ~ close_to_crossrail * post_treatment +
                          log_underground_distance + log_population_density + 
                          employment_rate + percent_households_deprived + 
                          terraced_binary + flat_binary,
                        data = sampled_data, listw = lw)

# Summarize the Spatial Error Model
summary(sem_model)

# Fit the Spatial Lag Model using sparse matrices
slm_model_sparse <- lagsarlm(log_price ~ close_to_crossrail + post_treatment +
                               log_underground_distance  +  log_population_density  + 
                               log_min_distance_to_caz  + flat_binary + semi_detached_binary +
                               avg_bedrooms + average_qualification_level_4,
                             data = sampled_data, listw = listw_sparse)
summary(slm_model_sparse)





# Run the DiD regression model
western_detached_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                               log_underground_distance  +  log_population_density  + 
                               log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                             data = subset(western_subset, property_type == "D"))

# Summarize the model
summary(western_detached_model)

#eastern detatched model
# Run the DiD regression model
eastern_detached_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                               log_underground_distance  +  log_population_density  + 
                               log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                             data = subset(eastern_subset, property_type == "D"))

# western flat
western_flat_model <- lm(log_price ~ close_to_crossrail + post_treatment +
                           log_underground_distance  +  log_population_density  + 
                           log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                         data = subset(western_subset, property_type == "F"))

eastern_flat_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                           log_underground_distance  +  log_population_density  + 
                           log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                         data = subset(eastern_subset, property_type == "F"))

# western semi-detached
western_semi_detached_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                                     log_underground_distance  +  log_population_density  + 
                                     log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                                   data = subset(western_subset, property_type == "S"))

# eastern semi-detached
eastern_semi_detached_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                                     log_underground_distance  +  log_population_density  + 
                                     log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                                   data = subset(eastern_subset, property_type == "S"))

# western terraced
western_terraced_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                               log_underground_distance  +  log_population_density  + 
                               log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                             data = subset(western_subset, property_type == "T"))

# eastern terraced
eastern_terraced_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                               log_underground_distance  +  log_population_density  + 
                               log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                             data = subset(eastern_subset, property_type == "T"))

# stargazer the detached models
stargazer(western_detached_model, eastern_detached_model,
          dep.var.labels = "Log Price",
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", 
                               "Population Density", "Distance to CAZ", "Avg Bedrooms", 
                               "Qualification Level 4", "Close to Crossrail * Post-Treatment"),
          type = "html", out = "detached_flat_models.htm")

stargazer(western_flat_model, eastern_flat_model,
          type = "html",
          dep.var.labels = "Log Price",
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", 
                               "Population Density", "Distance to CAZ", "Avg Bedrooms", 
                               "Qualification Level 4", "Close to Crossrail * Post-Treatment"),
          out = "flat_models.htm")

stargazer(western_semi_detached_model, eastern_semi_detached_model,
          dep.var.labels = "Log Price",
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", 
                               "Population Density", "Distance to CAZ", "Avg Bedrooms", 
                               "Qualification Level 4", "Close to Crossrail * Post-Treatment"),
          type = "html", out = "semi_detached_models.htm")

stargazer(western_terraced_model, eastern_terraced_model,
          dep.var.labels = "Log Price",
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", 
                               "Population Density", "Distance to CAZ", "Avg Bedrooms", 
                               "Qualification Level 4", "Close to Crossrail * Post-Treatment"),
          type = "html", out = "terraced_models.htm")





# central and whole model 
central_detached_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                               log_underground_distance  +  log_population_density  + 
                               log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                             data = subset(central_subset, property_type == "D"))

central_flat_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                           log_underground_distance  +  log_population_density  + 
                           log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                         data = subset(central_subset, property_type == "F"))

central_semi_detached_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                                     log_underground_distance  +  log_population_density  + 
                                     log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                                   data = subset(central_subset, property_type == "S"))

central_terraced_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                               log_underground_distance  +  log_population_density  + 
                               log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                             data = subset(central_subset, property_type == "T"))


whole_detached_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                            log_underground_distance  +  log_population_density  + 
                            log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                          data = subset(properties_with_census, property_type == "D"))

whole_flat_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                        log_underground_distance  +  log_population_density  + 
                        log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                      data = subset(properties_with_census, property_type == "F"))

whole_semi_detached_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                                  log_underground_distance  +  log_population_density  + 
                                  log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                                data = subset(properties_with_census, property_type == "S"))

whole_terraced_model <- lm(log_price ~ close_to_crossrail * post_treatment +
                            log_underground_distance  +  log_population_density  + 
                            log_min_distance_to_caz + avg_bedrooms + average_qualification_level_4,
                          data = subset(properties_with_census, property_type == "T"))

stargazer(central_detached_model, central_flat_model, 
          dep.var.labels = "Log Price",
          column.labels = c("Detached", "Flat"),
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", 
                               "Population Density", "Distance to CAZ", "Avg Bedrooms", 
                               "Qualification Level 4", "Close to Crossrail * Post-Treatment"),
          type = "html", out = "central_models1.htm")

stargazer(central_semi_detached_model, central_terraced_model,
          dep.var.labels = "Log Price",
          column.labels = c("Semi", "Terraced"),
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", 
                               "Population Density", "Distance to CAZ", "Avg Bedrooms", 
                               "Qualification Level 4", "Close to Crossrail * Post-Treatment"),
          type = "html", out = "central_models2.htm")

stargazer(whole_detached_model, whole_flat_model, 
          dep.var.labels = "Log Price",
          column.labels = c("Detached", "Flat"),
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", 
                               "Population Density", "Distance to CAZ", "Avg Bedrooms", 
                               "Qualification Level 4", "Close to Crossrail * Post-Treatment"),
          type = "html", out = "whole_models.htm")

stargazer(whole_semi_detached_model, whole_terraced_model,
          dep.var.labels = "Log Price",
          column.labels = c("Semi", "Terraced"),
          covariate.labels = c("Close to Crossrail", "Post-Treatment", "Underground Distance", 
                               "Population Density", "Distance to CAZ", "Avg Bedrooms", 
                               "Qualification Level 4", "Close to Crossrail * Post-Treatment"),
          type = "html", out = "whole_models2.htm")




resettest(western_detached_model, power = 2:3, type = "fitted")
resettest(eastern_detached_model, power = 2:3, type = "fitted")
resettest(western_flat_model, power = 2:3, type = "fitted")
resettest(eastern_flat_model, power = 2:3, type = "fitted")
resettest(western_semi_detached_model, power = 2:3, type = "fitted")
resettest(eastern_semi_detached_model, power = 2:3, type = "fitted")
resettest(western_terraced_model, power = 2:3, type = "fitted")
resettest(eastern_terraced_model, power = 2:3, type = "fitted")

resettest(central_detached_model, power = 2:3, type = "fitted")
resettest(central_flat_model, power = 2:3, type = "fitted")
resettest(central_semi_detached_model, power = 2:3, type = "fitted")
resettest(central_terraced_model, power = 2:3, type = "fitted")
resettest(whole_detached_model, power = 2:3, type = "fitted")
resettest(whole_flat_model, power = 2:3, type = "fitted")
resettest(whole_semi_detached_model, power = 2:3, type = "fitted")
resettest(whole_terraced_model, power = 2:3, type = "fitted")

# Load necessary package
install.packages("stargazer")
library(stargazer)

# Create a data frame with the RESET test results
reset_test_results <- data.frame(
  Model = c("Western Detached", "Eastern Detached", "Central Detached", "Whole Detached",
            "Western Flat", "Eastern Flat", "Central Flat", "Whole Flat",
            "Western Semi-Detached", "Eastern Semi-Detached", "Central Semi-Detached", "Whole Semi-Detached",
            "Western Terraced", "Eastern Terraced", "Central Terraced", "Whole Terraced"),
  RESET = c(28.6, 51.618, 6.0027, 53.696, 
            209.75, 440.99, 357.46, 2860.4, 
            137.61, 89.877, 29.44, 627.79, 
            445.75, 358.55, 564.68, 3011.6),
  df1 = c(2, 2, 2, 2, 
          2, 2, 2, 2, 
          2, 2, 2, 2, 
          2, 2, 2, 2),
  df2 = c(7757, 5070, 202, 17919, 
          39941, 48867, 83240, 246917, 
          19176, 18620, 626, 46302, 
          31921, 53142, 10639, 106029),
  p_value = c("4.215e-13", "< 2.2e-16", "0.002935", "< 2.2e-16", 
              "< 2.2e-16", "< 2.2e-16", "< 2.2e-16", "< 2.2e-16", 
              "< 2.2e-16", "< 2.2e-16", "6.034e-13", "< 2.2e-16", 
              "< 2.2e-16", "< 2.2e-16", "< 2.2e-16", "< 2.2e-16")
)

# Print the table using stargazer
stargazer(reset_test_results, type = "html", summary = FALSE, rownames = FALSE,
          title = "Comparison of RESET Tests for Different Models",
          column.labels = c("Model", "RESET", "df1", "df2", "p-value"),
          out="full_reset_test_results.htm")

# Create Q-Q plots for the four models in a 2x2 layout
par(mfrow = c(2, 2))  # Set up a 2x2 plotting layout

# Q-Q plot for western_detached_model
qqnorm(western_detached_model$residuals, main = "Q-Q Plot for Western Detached")
qqline(western_detached_model$residuals, col = "red")

# Q-Q plot for eastern_detached_model
qqnorm(western_flat_model$residuals, main = "Q-Q Plot for Western Flat")
qqline(western_flat_model$residuals, col = "red")

# Q-Q plot for western_semi_detached_model
qqnorm(western_semi_detached_model$residuals, main = "Q-Q Plot for Western Semi-Detached")
qqline(western_semi_detached_model$residuals, col = "red")

# Q-Q plot for western_terraced_model
qqnorm(western_terraced_model$residuals, main = "Q-Q Plot for Western Terraced")
qqline(western_terraced_model$residuals, col = "red")

# Reset to default layout
par(mfrow = c(1, 1))



crPlots(western_flat_model)



