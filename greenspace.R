# load csv
greenspace <- read.csv("data/access_parks_walk.csv")

# rename geo_code to LSOA11CD
greenspace <- greenspace %>%
  rename(LSOA11CD = geo_code)

#select LSOA11CD and access_parks_30
greenspace <- greenspace %>%
  select(LSOA11CD, access_parks_30_pct)

# save as RData
save(greenspace, file = "greenspace.RData")
