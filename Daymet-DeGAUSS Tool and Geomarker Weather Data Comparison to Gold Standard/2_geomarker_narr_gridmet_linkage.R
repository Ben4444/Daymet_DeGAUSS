# Loading necessary packages
library(tidyverse)
library(geomarker)
library(dht)

# Importing the NOAA stations dataset
noaa_stations <- read_csv("noaa_stations.csv")

# Dropping the ID and station variables and NOAA station measures
noaa_stations <- noaa_stations %>%
  select(-c(id, station, tmax_noaa, tmin_noaa))

# Expanding the dates between start_date and end_date
noaa_stations$start_date <- check_dates(noaa_stations$start_date, allow_missing = TRUE)
noaa_stations$end_date <- check_dates(noaa_stations$end_date, allow_missing = TRUE)
noaa_stations <- expand_dates(noaa_stations, by = "day") %>%
  select(-c(start_date, end_date))

# Sorting by station name and measure date
noaa_stations <- noaa_stations %>%
  arrange(name, date)

# Creating a full-resolution S2 cell column, from lat and lon
noaa_stations$s2_cell <- s2::as_s2_cell(s2::s2_lnglat(noaa_stations$lon, noaa_stations$lat))
noaa_stations <- noaa_stations %>%
  select(-c(lat, lon))

# Transposing dates into a list, by station name
noaa_stations <- noaa_stations %>%
  group_by(name) %>%
  mutate(var = row_number()) %>%
  ungroup()
noaa_stations_wide <- noaa_stations %>%
  group_by(name) %>%
  pivot_wider(names_from = var, names_prefix = "date_", values_from = date) %>%
  ungroup()
noaa_stations_wide <- noaa_stations_wide %>%
  rowwise() %>%
  mutate(dates = list(c_across(starts_with("date_")))) %>%
  ungroup()
noaa_stations_wide <- noaa_stations_wide %>%
  select(name, s2_cell, dates)
noaa_stations_wide <- noaa_stations_wide %>%
  mutate(dates = map(dates, ~ .x[!is.na(.x)]))

# Creating an s2_cell_dates vector
noaa_stations_s2cd <- as_s2cd(noaa_stations_wide)
rm(noaa_stations_wide)

# Linking in NARR air temperature data
noaa_stations_narr <- get_narr_data(noaa_stations_s2cd, narr_var = "air.2m")

# Transposing the NARR air temperature data
for (i in 1:length(noaa_stations_narr)) {
  noaa_stations_narr_df <- as.data.frame(noaa_stations_narr[i])
  noaa_stations_narr_df <- as.data.frame(t(noaa_stations_narr_df))
  noaa_stations_narr_df <- rownames_to_column(noaa_stations_narr_df, var = "s2_cell")
  noaa_stations_narr_df <- noaa_stations_narr_df %>%
    rename_with(~ gsub("^V", "narr", .x))
  assign(paste0("narr_", i), noaa_stations_narr_df)
}
rm(noaa_stations_narr_df, i, noaa_stations_narr)

# Linking in gridMET surface temperature data
noaa_stations_gridmet_tmmx <- get_gridmet_data(noaa_stations_s2cd, gridmet_var = "tmmx")
noaa_stations_gridmet_tmmn <- get_gridmet_data(noaa_stations_s2cd, gridmet_var = "tmmn")
rm(noaa_stations_s2cd)

# Transposing the gridMET surface temperature data
for (i in 1:length(noaa_stations_gridmet_tmmx)) {
  noaa_stations_gridmet_df <- as.data.frame(noaa_stations_gridmet_tmmx[i])
  noaa_stations_gridmet_df <- as.data.frame(t(noaa_stations_gridmet_df))
  noaa_stations_gridmet_df <- rownames_to_column(noaa_stations_gridmet_df, var = "s2_cell")
  noaa_stations_gridmet_df <- noaa_stations_gridmet_df %>%
    rename_with(~ gsub("^V", "gridmet_tmmx", .x))
  assign(paste0("gridmet_tmmx_", i), noaa_stations_gridmet_df)
}
rm(noaa_stations_gridmet_df, i, noaa_stations_gridmet_tmmx)
for (i in 1:length(noaa_stations_gridmet_tmmn)) {
  noaa_stations_gridmet_df <- as.data.frame(noaa_stations_gridmet_tmmn[i])
  noaa_stations_gridmet_df <- as.data.frame(t(noaa_stations_gridmet_df))
  noaa_stations_gridmet_df <- rownames_to_column(noaa_stations_gridmet_df, var = "s2_cell")
  noaa_stations_gridmet_df <- noaa_stations_gridmet_df %>%
    rename_with(~ gsub("^V", "gridmet_tmmn", .x))
  assign(paste0("gridmet_tmmn_", i), noaa_stations_gridmet_df)
}
rm(noaa_stations_gridmet_df, i, noaa_stations_gridmet_tmmn)

# Processing the NARR air temperature data for a merge with the NOAA stations
noaa_stations_narr <- bind_rows(mget(ls(pattern = "^narr_")))
rm(list = ls(pattern = "^narr_[0-9]"))
noaa_stations_narr$s2_cell <- str_remove_all(noaa_stations_narr$s2_cell, "^X")
noaa_stations_narr <- noaa_stations_narr %>%
  pivot_longer(
    cols = starts_with("narr"),
    names_to = "var",
    values_to = "narr")
noaa_stations_narr$var <- str_remove_all(noaa_stations_narr$var, "narr")
noaa_stations_narr$var <- as.integer(noaa_stations_narr$var)
noaa_stations_narr <- noaa_stations_narr %>%
  filter(!is.na(narr))

# Processing the gridMET surface temperature data for a merge with the NOAA stations
noaa_stations_gridmet_tmmx <- bind_rows(mget(ls(pattern = "^gridmet_tmmx_")))
rm(list = ls(pattern = "^gridmet_tmmx_[0-9]"))
noaa_stations_gridmet_tmmx$s2_cell <- str_remove_all(noaa_stations_gridmet_tmmx$s2_cell, "^X")
noaa_stations_gridmet_tmmx <- noaa_stations_gridmet_tmmx %>%
  pivot_longer(
    cols = starts_with("gridmet_tmmx"),
    names_to = "var",
    values_to = "gridmet_tmmx")
noaa_stations_gridmet_tmmx$var <- str_remove_all(noaa_stations_gridmet_tmmx$var, "gridmet_tmmx")
noaa_stations_gridmet_tmmx$var <- as.integer(noaa_stations_gridmet_tmmx$var)
noaa_stations_gridmet_tmmx <- noaa_stations_gridmet_tmmx %>%
  filter(!is.na(gridmet_tmmx))
noaa_stations_gridmet_tmmn <- bind_rows(mget(ls(pattern = "^gridmet_tmmn_")))
rm(list = ls(pattern = "^gridmet_tmmn_[0-9]"))
noaa_stations_gridmet_tmmn$s2_cell <- str_remove_all(noaa_stations_gridmet_tmmn$s2_cell, "^X")
noaa_stations_gridmet_tmmn <- noaa_stations_gridmet_tmmn %>%
  pivot_longer(
    cols = starts_with("gridmet_tmmn"),
    names_to = "var",
    values_to = "gridmet_tmmn")
noaa_stations_gridmet_tmmn$var <- str_remove_all(noaa_stations_gridmet_tmmn$var, "gridmet_tmmn")
noaa_stations_gridmet_tmmn$var <- as.integer(noaa_stations_gridmet_tmmn$var)
noaa_stations_gridmet_tmmn <- noaa_stations_gridmet_tmmn %>%
  filter(!is.na(gridmet_tmmn))

# Merging the NARR and gridMET temperature data with the NOAA stations
noaa_stations$s2_cell <- as.character(noaa_stations$s2_cell)
noaa_stations_narr_gridmet <- noaa_stations %>%
  inner_join(noaa_stations_narr, by = c("s2_cell", "var")) %>%
  inner_join(noaa_stations_gridmet_tmmx, by = c("s2_cell", "var")) %>%
  inner_join(noaa_stations_gridmet_tmmn, by = c("s2_cell", "var"))
rm(noaa_stations, noaa_stations_narr, noaa_stations_gridmet_tmmx, noaa_stations_gridmet_tmmn)
noaa_stations_narr_gridmet <- noaa_stations_narr_gridmet %>%
  select(-c(s2_cell, var))
noaa_stations_narr_gridmet <- noaa_stations_narr_gridmet %>%
  rename(tavg_narr = narr,
         tmax_gridmet = gridmet_tmmx,
         tmin_gridmet = gridmet_tmmn)

# Writing the linked NARR and gridMET temperature data data out
write_csv(noaa_stations_narr_gridmet, "noaa_stations_narr_gridmet.csv")