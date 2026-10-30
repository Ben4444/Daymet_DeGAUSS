# Loading necessary packages
library(tidyverse)
library(geomarker)
library(dht)

# Importing the Chicago 2020 census tract centroids dataset
centroids <- read_csv("centroids.csv")

# Dropping the ID variable
centroids <- centroids %>%
  select(-id)

# Expanding the dates between start_date and end_date
centroids$start_date <- check_dates(centroids$start_date, allow_missing = TRUE)
centroids$end_date <- check_dates(centroids$end_date, allow_missing = TRUE)
centroids <- expand_dates(centroids, by = "day") %>%
  select(-c(start_date, end_date))

# Creating a full-resolution S2 cell column, from lat and lon
centroids$s2_cell <- s2::as_s2_cell(s2::s2_lnglat(centroids$lon, centroids$lat))
centroids <- centroids %>%
  select(-c(lat, lon))

# Transposing dates into a list, by census tract
centroids <- centroids %>%
  group_by(tract_full) %>%
  mutate(var = row_number()) %>%
  ungroup()
centroids_wide <- centroids %>%
  group_by(tract_full) %>%
  pivot_wider(names_from = var, names_prefix = "date_", values_from = date) %>%
  ungroup()
centroids_wide <- centroids_wide %>%
  rowwise() %>%
  mutate(dates = list(c_across(starts_with("date_")))) %>%
  ungroup()
centroids_wide <- centroids_wide %>%
  select(tract_full, s2_cell, dates)
centroids_wide <- centroids_wide %>%
  mutate(dates = map(dates, ~ .x[!is.na(.x)]))

# Creating an s2_cell_dates vector
centroids_s2cd <- as_s2cd(centroids_wide)
rm(centroids_wide)

# Linking in gridMET surface temperature data
centroids_gridmet_tmmx <- get_gridmet_data(centroids_s2cd, gridmet_var = "tmmx")
centroids_gridmet_tmmn <- get_gridmet_data(centroids_s2cd, gridmet_var = "tmmn")
rm(centroids_s2cd)

# Transposing the gridMET surface temperature data
for (i in 1:length(centroids_gridmet_tmmx)) {
  centroids_gridmet_df <- as.data.frame(centroids_gridmet_tmmx[i])
  centroids_gridmet_df <- as.data.frame(t(centroids_gridmet_df))
  centroids_gridmet_df <- rownames_to_column(centroids_gridmet_df, var = "s2_cell")
  centroids_gridmet_df <- centroids_gridmet_df %>%
    rename_with(~ gsub("^V", "gridmet_tmmx", .x))
  assign(paste0("gridmet_tmmx_", i), centroids_gridmet_df)
}
rm(centroids_gridmet_df, i, centroids_gridmet_tmmx)
for (i in 1:length(centroids_gridmet_tmmn)) {
  centroids_gridmet_df <- as.data.frame(centroids_gridmet_tmmn[i])
  centroids_gridmet_df <- as.data.frame(t(centroids_gridmet_df))
  centroids_gridmet_df <- rownames_to_column(centroids_gridmet_df, var = "s2_cell")
  centroids_gridmet_df <- centroids_gridmet_df %>%
    rename_with(~ gsub("^V", "gridmet_tmmn", .x))
  assign(paste0("gridmet_tmmn_", i), centroids_gridmet_df)
}
rm(centroids_gridmet_df, i, centroids_gridmet_tmmn)

# Processing the gridMET surface temperature data for a merge with the census tract centroids
centroids_gridmet_tmmx <- bind_rows(mget(ls(pattern = "^gridmet_tmmx_")))
rm(list = ls(pattern = "^gridmet_tmmx_[0-9]"))
centroids_gridmet_tmmx$s2_cell <- str_remove_all(centroids_gridmet_tmmx$s2_cell, "^X")
centroids_gridmet_tmmx <- centroids_gridmet_tmmx %>%
  pivot_longer(
    cols = starts_with("gridmet_tmmx"),
    names_to = "var",
    values_to = "gridmet_tmmx")
centroids_gridmet_tmmx$var <- str_remove_all(centroids_gridmet_tmmx$var, "gridmet_tmmx")
centroids_gridmet_tmmx$var <- as.integer(centroids_gridmet_tmmx$var)
centroids_gridmet_tmmx <- centroids_gridmet_tmmx %>%
  filter(!is.na(gridmet_tmmx))
centroids_gridmet_tmmn <- bind_rows(mget(ls(pattern = "^gridmet_tmmn_")))
rm(list = ls(pattern = "^gridmet_tmmn_[0-9]"))
centroids_gridmet_tmmn$s2_cell <- str_remove_all(centroids_gridmet_tmmn$s2_cell, "^X")
centroids_gridmet_tmmn <- centroids_gridmet_tmmn %>%
  pivot_longer(
    cols = starts_with("gridmet_tmmn"),
    names_to = "var",
    values_to = "gridmet_tmmn")
centroids_gridmet_tmmn$var <- str_remove_all(centroids_gridmet_tmmn$var, "gridmet_tmmn")
centroids_gridmet_tmmn$var <- as.integer(centroids_gridmet_tmmn$var)
centroids_gridmet_tmmn <- centroids_gridmet_tmmn %>%
  filter(!is.na(gridmet_tmmn))

# Merging the gridMET temperature data with the census tract centroids
centroids$s2_cell <- as.character(centroids$s2_cell)
centroids_gridmet <- centroids %>%
  inner_join(centroids_gridmet_tmmx, by = c("s2_cell", "var")) %>%
  inner_join(centroids_gridmet_tmmn, by = c("s2_cell", "var"))
rm(centroids, centroids_gridmet_tmmx, centroids_gridmet_tmmn)
centroids_gridmet <- centroids_gridmet %>%
  select(-c(s2_cell, var))
centroids_gridmet <- centroids_gridmet %>%
  rename(tmax_gridmet = gridmet_tmmx,
         tmin_gridmet = gridmet_tmmn)

# Writing the linked gridMET temperature data data out
write_csv(centroids_gridmet, "centroids_gridmet.csv")