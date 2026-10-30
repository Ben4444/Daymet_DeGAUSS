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

# Importing the linked gridMET temperature data
gridmet_data <- read_csv("centroids_gridmet.csv")

# Converting gridMET temperature from Kelvin to Celsius
gridmet_data <- gridmet_data %>%
  mutate(tmax_gridmet = tmax_gridmet - 273.15,
         tmin_gridmet = tmin_gridmet - 273.15)

# Merging the linked Daymet data with the linked gridMET data
centroids_daymet_gridmet <- inner_join(daymet_data, gridmet_data, by = c("tract_full", "date"))
rm(daymet_data, gridmet_data)

# Removing observations with any missing
centroids_daymet_gridmet <- centroids_daymet_gridmet[rowSums(is.na(centroids_daymet_gridmet)) == 0, ]

# Rounding the temperature variables to the hundreths decimal place to allow for a fair comparison
centroids_daymet_gridmet <- centroids_daymet_gridmet %>%
  mutate_at(vars(tmax_daymet, tmin_daymet, tmax_gridmet, tmin_gridmet), ~ round(., 2))

# Reordering the variables
centroids_daymet_gridmet <- centroids_daymet_gridmet %>%
  relocate(tract_full, date, tmax_daymet, tmin_daymet, tmax_gridmet, tmin_gridmet)
nrow(centroids_daymet_gridmet)
# 789 total census tract centroids

# Writing the results out as a CSV file
write_csv(centroids_daymet_gridmet, "centroids_daymet_gridmet.csv")