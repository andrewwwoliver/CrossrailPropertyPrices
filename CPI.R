###### CPI from https://www.ons.gov.uk/economy/inflationandpriceindices/datasets/consumerpriceinflation

# CPI uses yearly average from 2005 to 2023
# For 2024, monthly averages were used and weighted
## Weighting was based on average figures of January to July for 2005 to 2023, then compared to the annual averages
## The weighting was then applied to the monthly averages for 2024
## CPI was * 1.006206407 higher (0.6%) in Jan-July than the annual average

# Create a data frame with the CPI data
cpi <- data.frame(
  year = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024),
  CPI = c(90.1, 93.6, 96.0, 98.2, 99.6, 100.0, 101.0, 103.6, 106.0, 107.8, 108.9, 111.6, 120.5, 128.6, 132.7)
)

# View the CPI table
print(cpi)

#save as rdata
save(cpi, file = "CPI.RData")

# load CPI.RData
load("CPI.RData")

# Merge the CPI data with your existing data
properties_with_census <- merge(properties_with_census, cpi, by.x = "year", by.y = "year", all.x = TRUE)

# Calculate the adjustment factor
base_cpi <- cpi$CPI[cpi$year == 2010]
properties_with_census <- properties_with_census %>%
  mutate(adjustment_factor = base_cpi / CPI)

# Adjust the prices to 2010 pounds
properties_with_census <- properties_with_census %>%
  mutate(price_in_2010_pounds = price * adjustment_factor)

# View the updated data frame
head(properties_with_census)

#log 2010 pounds (+1 to avoid 0 ) 
properties_with_census <- properties_with_census %>%
  mutate(log_price_in_2010_pounds = log(price_in_2010_pounds + 1))

