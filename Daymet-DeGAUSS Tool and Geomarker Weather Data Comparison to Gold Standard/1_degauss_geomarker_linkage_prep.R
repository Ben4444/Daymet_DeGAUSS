# Loading necessary packages
library(tidyverse)

# Reading in the Chicago NOAA station data
noaa_stations <- read_csv("noaa_chicago_2017_2024.csv")

# Keeping only necessary variables
noaa_stations <- noaa_stations %>%
  select(-c(ELEVATION))

# Removing observations that are missing latitude or longitude
noaa_stations <- noaa_stations %>%
  filter(!is.na(LATITUDE) & !is.na(LONGITUDE))

# Removing observations that are missing TMAX or TMIN
noaa_stations <- noaa_stations %>%
  filter(!is.na(TMAX) & !is.na(TMIN))

# Checking the NOAA station temperature measures to see if any observations should be removed
min(noaa_stations$TMAX)
max(noaa_stations$TMAX)
min(noaa_stations$TMIN)
max(noaa_stations$TMIN)

# Checking the NOAA station temperature measure flags to see if any observations should be removed
table(noaa_stations$TMAX_ATTRIBUTES)
# Removing TMAX values that failed gap check or internal consistency check
noaa_stations <- noaa_stations %>%
  filter(!TMAX_ATTRIBUTES %in% c(",G,W", ",I,7", ",I,W"))

table(noaa_stations$TMIN_ATTRIBUTES)
# Removing TMIN values that failed internal consistency check or spatial consistency check
noaa_stations <- noaa_stations %>%
  filter(!TMIN_ATTRIBUTES %in% c(",I,W", ",S,W"))

noaa_stations <- noaa_stations %>%
  select(-c(TMAX_ATTRIBUTES, TMIN_ATTRIBUTES))

# Removing observations that are missing any values
noaa_stations <- noaa_stations[rowSums(is.na(noaa_stations)) == 0, ]

# Renaming variables
noaa_stations <- noaa_stations %>%
  rename(station = STATION,
         name = NAME,
         lat = LATITUDE,
         lon = LONGITUDE,
         date = DATE,
         tmax_noaa = TMAX,
         tmin_noaa = TMIN)

# Adding an ID variable
noaa_stations <- noaa_stations %>%
  mutate(id = row_number())

# Creating start_date and end_date variables
noaa_stations <- noaa_stations %>%
  mutate(start_date = date,
         end_date = date) %>%
  select(-date)

# Reordering variables
noaa_stations <- noaa_stations %>%
  relocate(id, station, name, lat, lon, start_date, end_date, tmax_noaa, tmin_noaa)

# Checking NOAA stations descriptives
nrow(noaa_stations)
# 8,519 station observations total
min(noaa_stations$start_date)
max(noaa_stations$end_date)
# Date range from January 1, 2017 to December 31, 2024 (2,922 days)
table(noaa_stations$name)
# 2,917 observations from the Chicago Midway Airport station
# 2,680 observations from the Chicago Northerly Island station
# 2,922 observations from the Chicago O'Hare International Airport station

# Writing the prepped NOAA station data out
write_csv(noaa_stations, "noaa_stations.csv")