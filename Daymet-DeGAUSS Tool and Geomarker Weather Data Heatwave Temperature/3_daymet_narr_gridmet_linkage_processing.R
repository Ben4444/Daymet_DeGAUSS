# Loading the necessary packages
library(tidyverse)

# Importing the linked Daymet temperature data
daymet_data <- read_csv("centroids_daymet.csv")

# Renaming the Daymet variables
daymet_data <- daymet_data %>%
  rename(tmax_daymet = tmax,
         tmin_daymet = tmin)

# Dropping unneeded variables
daymet_data <- daymet_data %>%
  select(-id)

# Importing the linked NARR and gridMET temperature data
narr_gridmet_data <- read_csv("centroids_narr_gridmet.csv")

# Converting NARR and gridMET temperature from Kelvin to Celsius
narr_gridmet_data <- narr_gridmet_data %>%
  mutate(tavg_narr = tavg_narr - 273.15,
         tmax_gridmet = tmax_gridmet - 273.15,
         tmin_gridmet = tmin_gridmet - 273.15)

# Merging the linked Daymet data with the linked NARR and gridMET data
centroids_daymet_narr_gridmet <- inner_join(daymet_data, narr_gridmet_data, by = c("tract_full", "date"))
rm(daymet_data, narr_gridmet_data)

# Removing observations with any missing
centroids_daymet_narr_gridmet <- centroids_daymet_narr_gridmet[rowSums(is.na(centroids_daymet_narr_gridmet)) == 0, ]

# So that they can be compared against NARR, creating average temperature variables for Daymet and gridMET
# Using formula: tavg = 0.606tmax + 0.394tmin
# This comes from: Running et al., 1987, Extrapolation of synoptic meteorological data in mountainous terrain and its use for simulating forest evapotranspiration and photosynthesis
centroids_daymet_narr_gridmet <- centroids_daymet_narr_gridmet %>%
  mutate(tavg_daymet = (0.606 * tmax_daymet) + (0.394 * tmin_daymet),
         tavg_gridmet = (0.606 * tmax_gridmet) + (0.394 * tmin_gridmet)) %>%
  select(-c(tmax_daymet, tmin_daymet, tmax_gridmet, tmin_gridmet))

# Rounding the temperature variables to the tenths decimal place to allow for a fair comparison
centroids_daymet_narr_gridmet <- centroids_daymet_narr_gridmet %>%
  mutate_at(vars(tavg_narr, tavg_daymet, tavg_gridmet), ~ round(., 1))

# Reordering the variables
centroids_daymet_narr_gridmet <- centroids_daymet_narr_gridmet %>%
  relocate(tract_full, date, tavg_daymet, tavg_narr, tavg_gridmet)
nrow(centroids_daymet_narr_gridmet)
# 789 total census tract centroids

# Writing the results out as a CSV file
write_csv(centroids_daymet_narr_gridmet, "centroids_daymet_narr_gridmet.csv")