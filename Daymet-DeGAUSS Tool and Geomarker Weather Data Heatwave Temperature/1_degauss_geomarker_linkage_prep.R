# Loading necessary packages
library(tidyverse)
library(sf)

# Loading Chicago 2020 census tract boundaries shapefile
census_tracts <- st_read(dsn = "Boundaries - Census Tracts - 2020_20261023/geo_export_97d0a9fa-91fb-41d0-b1ff-52a689dee70a.shp", quiet = TRUE)

# Dropping unneeded variables
census_tracts <- census_tracts %>%
  select(-c(tract_fips, tract_ce, commarea, commarea_n, community_))

# Extracting the centroid of each Chicago census tract
centroids <- st_centroid(census_tracts)

# Getting the latitude and longitude coordinates of each census tract centroid
coords <- st_coordinates(centroids)
centroids$lat <- coords[, "Y"]
centroids$lon <- coords[, "X"]

# Dropping unneeded variables
centroids <- as.data.frame(centroids)
centroids <- centroids %>%
  select(-geometry)

# Adding an id variable
centroids <- centroids %>%
  mutate(id = row_number())

# During the 1995 Chicago heatwave (deadliest weather event in Chicago history), July 13 was the hottest day (https://www.weather.gov/lot/1995_heatwave)
# Therefore, for this comparison, linking in temperature data on July 13, 1995
# Adding start_date and end_date variables for this date
centroids <- centroids %>%
  mutate(start_date = as_date("1995-07-13"),
         end_date = as_date("1995-07-13"))

# Reordering the variables
centroids <- centroids %>%
  relocate(id, tract_full, lat, lon, start_date, end_date)
nrow(centroids)
# 789 total census tract centroids

# Writing the prepped census tract centroid data out
write_csv(centroids, "centroids.csv")