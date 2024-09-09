library(tidyr)
library(ggspatial)





# Extract the random effects
random_effects <- ranef(flat_lmer_model)

# Extract random effects for LSOA11CD
lsoa_random_effects <- random_effects$LSOA11CD

# Convert to a dataframe
lsoa_random_effects_df <- as.data.frame(lsoa_random_effects)

# Add LSOA identifiers as a column
lsoa_random_effects_df$LSOA11CD <- rownames(lsoa_random_effects)

# View the dataframe
head(lsoa_random_effects_df)

#Read GeoJSON LSOA into spatial df
lsoa <- st_read("LSOA_2011_Boundaries.geojson")

#select LSOA11CD and geometry
lsoa <- lsoa %>% select(LSOA11CD, geometry)

#link random effects to LSOA
lsoa <- left_join(lsoa, lsoa_random_effects_df, by = "LSOA11CD")

#remove any nAS in post_treatment column
lsoa <- lsoa %>%
  drop_na(post_treatment)




head(lsoa)

library(ggplot2)
library(sf)

# Plot the map for the random slopes
ggplot(data = lsoa) +
  geom_sf(aes(fill = `(Intercept)`), color = NA) +  # Fill map with random slopes for post_treatment
  scale_fill_viridis_c(option = "plasma", na.value = "gray90", name = "Random Slope (Intercept)") +  # Color scale
  labs(title = "Choropleth Map of Random Slopes (Intercept) by LSOA",
       subtitle = "Random effects from the mixed-effects model",
       caption = "Source: LSOA Boundaries and Model Random Effects") +
  theme_minimal() +
  theme(legend.position = "right") 

# Create the choropleth map with a basemap
ggplot(data = lsoa) +
  annotation_map_tile(type = "cartolight") +  # Add a basemap (you can choose other types like "osm")
  geom_sf(aes(fill = `(Intercept)`), color = NA) +  # Fill map with random slopes for post_treatment
  scale_fill_viridis_c(option = "plasma", na.value = "gray90", name = "Random Slope (Intercept)") +  # Color scale
  labs(title = "Choropleth Map of Random Slopes (Intercept) by LSOA",
       subtitle = "Random effects from the mixed-effects model",
       caption = "Source: LSOA Boundaries and Model Random Effects") +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_sf(crs = st_crs(lsoa))  # Ensure the map uses the correct CRS


# Calculate the specific breaks at 20%, 40%, 60%, 80% of the range
min_intercept <- min(lsoa$`(Intercept)`, na.rm = TRUE)
max_intercept <- max(lsoa$`(Intercept)`, na.rm = TRUE)

# Define the breakpoints
breaks <- c(min_intercept,
            min_intercept + 0.2 * (max_intercept - min_intercept),
            min_intercept + 0.4 * (max_intercept - min_intercept),
            min_intercept + 0.6 * (max_intercept - min_intercept),
            min_intercept + 0.8 * (max_intercept - min_intercept),
            max_intercept)

# Create quantiles based on these breaks
lsoa <- lsoa %>%
  mutate(intercept_quantile = cut(`(Intercept)`, breaks = breaks, include.lowest = TRUE))

# Create labels for the quantiles
quantile_labels <- paste0(round(breaks[-6], 2), " - ", round(breaks[-1], 2))

# Create the choropleth map with a basemap and specified quantile ranges in the legend
ggplot(data = lsoa) +
  annotation_map_tile(type = "cartolight") +  # Add a basemap (you can choose other types like "osm")
  geom_sf(aes(fill = intercept_quantile), color = NA) +  # Fill map with quantile categories
  scale_fill_viridis_d(option = "plasma", na.value = "gray90", name = "Random Intercept (Quantiles)",
                       labels = quantile_labels) +  # Discrete color scale with quantile ranges
  labs(title = "Choropleth Map of Random Intercepts by LSOA (Specified Quantiles)",
       subtitle = "Random effects from the mixed-effects model",
       caption = "Source: LSOA Boundaries and Model Random Effects") +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_sf(crs = st_crs(lsoa))  # Ensure the map uses the correct CRS


################

# properties with census
employment <- properties_with_census %>%
  select(LSOA11CD, employment_pct_60)

# remove duplicates
employment <- employment %>%
  distinct()
employment_df <- employment

lsoa <- employment_df
#set as sf and crs to 4326
lsoa <- st_as_sf(lsoa, crs = 4326)

#Read GeoJSON LSOA into spatial df
lsoa <- st_read("LSOA_2011_Boundaries.geojson")

#select LSOA11CD and geometry
lsoa <- lsoa %>% select(LSOA11CD, geometry)

#join the geometries from lsoa to lsoa df
lsoa <- left_join(lsoa, employment_df, by = "LSOA11CD")




library(ggplot2)
library(sf)


# Calculate the specific breaks at 20%, 40%, 60%, 80% of the range
min_intercept <- min(lsoa$`employment_pct_60`, na.rm = TRUE)
max_intercept <- max(lsoa$`employment_pct_60`, na.rm = TRUE)

# Define the breakpoints
breaks <- c(min_intercept,
            min_intercept + 0.2 * (max_intercept - min_intercept),
            min_intercept + 0.4 * (max_intercept - min_intercept),
            min_intercept + 0.6 * (max_intercept - min_intercept),
            min_intercept + 0.8 * (max_intercept - min_intercept),
            max_intercept)

# Create quantiles based on these breaks
lsoa <- lsoa %>%
  mutate(intercept_quantile = cut(`employment_pct_60`, breaks = breaks, include.lowest = TRUE))

# Create labels for the quantiles
quantile_labels <- paste0(round(breaks[-6], 2), " - ", round(breaks[-1], 2))

# Create the choropleth map with a basemap and specified quantile ranges in the legend
ggplot(data = lsoa) +
  annotation_map_tile(type = "cartolight") +  # Add a basemap (you can choose other types like "osm")
  geom_sf(aes(fill = intercept_quantile), color = NA) +  # Fill map with quantile categories
  scale_fill_viridis_d(option = "plasma", na.value = "gray90", name = "Random Intercept (Quantiles)",
                       labels = quantile_labels) +  # Discrete color scale with quantile ranges
  labs(title = "Choropleth Map of Random Intercepts by LSOA (Specified Quantiles)",
       subtitle = "Random effects from the mixed-effects model",
       caption = "Source: LSOA Boundaries and Model Random Effects") +
  theme_minimal() +
  theme(legend.position = "right") +
  coord_sf(crs = st_crs(lsoa))  # Ensure the map uses the correct CRS

################

library(ggplot2)
library(ggspatial)
library(sf)
library(prettymapr)

# Assuming postcode_sf and ttm are already defined and processed
# Define the point you want to add as an sf object
cbd_point <- st_as_sf(data.frame(lon = -0.09480062, lat = 51.51144), 
                      coords = c("lon", "lat"), crs = 4326)

lsoa %>%
  ggplot() +
  # Add basemap
  annotation_map_tile(type = "cartolight", zoom = NULL) +
  geom_sf(aes(fill = employment_pct_60), color = NA) +
  geom_sf(data = cbd_point, aes(color = "Central Business District"), size = 3) +  # Map color to the CBD point
  scale_fill_viridis_c(direction = 1, name = "Travel Time") +  # Rename legend entry for travel time
  scale_color_manual(values = "red", name = NULL) +  # Custom legend entry for the CBD point
  annotation_north_arrow(location = "tl", which_north = "true",  # Move north arrow to top left
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) +  # Add north arrow at top left
  coord_sf(expand = FALSE) +  # Maintain correct coordinate system and add x/y axes
  theme_minimal() +  # Use a minimal theme to show axes
  theme(
    legend.text = element_text(face = "bold"),  # Make the legend text bold
    legend.spacing.y = unit(0.5, 'cm'),  # Adjust the vertical spacing between legend items
    panel.grid.major = element_line(color = "gray90"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )


library(ggplot2)
library(ggspatial)
library(sf)
library(prettymapr)

# Assuming lsoa has already been joined with the employment data and has the geometry

# Calculate the specific breaks at 20%, 40%, 60%, 80% of the range
min_pct <- min(lsoa$`employment_pct_60`, na.rm = TRUE)
max_pct <- max(lsoa$`employment_pct_60`, na.rm = TRUE)

# Define the breakpoints
breaks <- c(min_pct,
            min_pct + 0.2 * (max_pct - min_pct),
            min_pct + 0.4 * (max_pct - min_pct),
            min_pct + 0.6 * (max_pct - min_pct),
            min_pct + 0.8 * (max_pct - min_pct),
            max_pct)

# Create quantiles based on these breaks
lsoa <- lsoa %>%
  mutate(employment_quantile = cut(`employment_pct_60`, breaks = breaks, include.lowest = TRUE))

# Create labels for the quantiles
quantile_labels <- paste0(round(breaks[-6], 2), " - ", round(breaks[-1], 2))

# Plot the map
ggplot(data = lsoa) +
  annotation_map_tile(type = "cartolight", zoom = NULL) +  # Add basemap
  geom_sf(aes(fill = employment_quantile), color = NA) +  # Fill map with quantile categories
  scale_fill_viridis_d(option = "plasma", na.value = "gray90", name = "Employment Pct (Quantiles)",
                       labels = quantile_labels) +  # Discrete color scale with quantile ranges
  geom_sf(data = cbd_point, aes(color = "Central Business District"), size = 3) +  # Highlight CBD
  scale_color_manual(values = "red", name = NULL) +  # Custom legend entry for the CBD point
  annotation_north_arrow(location = "tl", which_north = "true",  # North arrow
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_fancy_orienteering) +  # Add north arrow at top left
  coord_sf(expand = FALSE) +  # Maintain correct coordinate system
  theme_minimal() +  # Use a minimal theme
  theme(
    legend.text = element_text(face = "bold"),  # Make the legend text bold
    legend.spacing.y = unit(0.5, 'cm'),  # Adjust the vertical spacing between legend items
    panel.grid.major = element_line(color = "gray90"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12)
  )
