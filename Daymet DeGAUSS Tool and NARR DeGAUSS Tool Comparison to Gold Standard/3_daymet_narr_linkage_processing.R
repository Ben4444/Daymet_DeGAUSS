# Loading the necessary packages
library(tidyverse)

# Importing the linked Daymet temperature data
daymet_data <- read_csv("noaa_stations_daymet.csv")

# Renaming the Daymet variables
daymet_data <- daymet_data %>%
  rename(tmax_daymet = tmax,
         tmin_daymet = tmin)

# Reordering the variables
daymet_data <- daymet_data %>%
  relocate(id, station, name, date, tmax_noaa, tmin_noaa, tmax_daymet, tmin_daymet)

# Because the NOAA temperature variables are to the tenths decimal place,
# and the Daymet temperature variables are to the hundreths decimal place,
# rounding the Daymet variables to the tenths decimal place to allow for a fair comparison
daymet_data <- daymet_data %>%
  mutate_at(vars(tmax_daymet, tmin_daymet), ~ round(., 1))

# Removing duplicates by station and date (duplicates are from leap years: 12/31 to 12/30 in Daymet linkage)
daymet_data <- daymet_data %>%
  distinct(station, name, date, .keep_all = TRUE)

# Importing the linked NARR temperature data
narr_data <- read_csv("noaa_stations_narr_1.0.0_weather.csv")

# Limiting the NARR data down to just needed variables
narr_data <- narr_data %>%
  select(id, air.2m)

# Renaming the NARR temperature variable
narr_data <- narr_data %>%
  rename(tavg_narr = air.2m)

# Converting NARR temperature from Kelvin to Celsius
narr_data <- narr_data %>%
  mutate(tavg_narr = tavg_narr - 273.15)

# Rounding NARR temperature to one decimal place to allow for a fair comparison
narr_data <- narr_data %>%
  mutate_at(vars(tavg_narr), ~ round(., 1))

# Merging the linked Daymet data with the linked NARR data
noaa_stations_daymet_narr <- inner_join(daymet_data, narr_data, by = "id")
rm(daymet_data, narr_data)

# Removing the id variable
noaa_stations_daymet_narr <- noaa_stations_daymet_narr %>%
  select(-id)

# Removing observations with any missing
noaa_stations_daymet_narr <- noaa_stations_daymet_narr[rowSums(is.na(noaa_stations_daymet_narr)) == 0, ]

# So that they can be compared against NARR, creating average temperature variables for NOAA and Daymet
# Using formula: tavg = 0.606tmax + 0.394tmin
# This comes from: Running et al., 1987, Extrapolation of synoptic meteorological data in mountainous terrain and its use for simulating forest evapotranspiration and photosynthesis
noaa_stations_daymet_narr <- noaa_stations_daymet_narr %>%
  mutate(tavg_noaa = (0.606 * tmax_noaa) + (0.394 * tmin_noaa),
         tavg_daymet = (0.606 * tmax_daymet) + (0.394 * tmin_daymet))
noaa_stations_daymet_narr <- noaa_stations_daymet_narr %>%
  mutate_at(vars(tavg_noaa, tavg_daymet), ~ round(., 1)) %>%
  select(-c(tmax_noaa, tmin_noaa, tmax_daymet, tmin_daymet))

# Reordering the variables
noaa_stations_daymet_narr <- noaa_stations_daymet_narr %>%
  relocate(station, name, date, tavg_noaa, tavg_daymet, tavg_narr)

# Checking NOAA stations descriptives
nrow(noaa_stations_daymet_narr)
# 8,514 station observations total
min(noaa_stations_daymet_narr$date)
max(noaa_stations_daymet_narr$date)
# Date range from January 1, 2017 to December 30, 2024 (2,920 days: no 12/31 on leap years)
table(noaa_stations_daymet_narr$name)
# 2,915 observations from the Chicago Midway Airport station
# 2,679 observations from the Chicago Northerly Island station
# 2,920 observations from the Chicago O'Hare International Airport station

# Writing the results out as a CSV file
write_csv(noaa_stations_daymet_narr, "noaa_stations_daymet_narr.csv")