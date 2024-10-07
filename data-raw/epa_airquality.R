library(sf)
library(stars)
library(dplyr)
library(tidyr)

## map data
.maps <- readRDS(file.path("sapphirine_data", "RDS", "map.RDS"))

## Map between pollutant standard and data field
map_standard_to_field <- function(standard) {
  switch(
    standard,
    "CO_1_hour_1971" = "second_max_value",
    "CO_8_hour_1971" = "second_max_nonoverlap_value",
    "SO2_1_hour_2010" = "ninety_ninth_percentile",
    "NO2_1_hour_2010" = "ninety_eighth_percentile",
    "NO2_annual_1971" = "arithmetic_mean",
    "Ozone_8_hour_2015" = "fourth_max_value",
    "PM10_24_hour_2006" = "primary_exceedance_count",
    "PM2.5_24_hour_2012" = "ninety_eighth_percentile",
    "PM2.5_annual_2012" = "arithmetic_mean"
  )
}

## Format function for cropped grid
format_grid <- function(x, file_dir) {
  meta_info <- unlist(strsplit(file_dir, "/"))
  x <- st_set_dimensions(x, "band", names = "year")
  ## x <- c(x, along = list("pollutant_standard" = meta_info[1]))
  x <- setNames(x, meta_info[1])
  if (grepl("mean", meta_info[2])) {
    data_field <- "arithmetic_mean"
  } else {
    data_field <- map_standard_to_field(meta_info[1])
  }
  x <- c(x, along = list("data_field" = data_field))
  if (grepl("included", meta_info[3])) {
    event <- "Events Included"
  } else {
    event <- "Events Excluded"
  }
  x <- c(x, along = list("event" = event))
  x
}

## Format function for summarized sf via areal-weight interpolation
format_aw <- function(x, file_dir) {
  pollutant_standard <- unlist(strsplit(file_dir, "/"))[1]
  x$POLLUTANT_STANDARD <- pollutant_standard
  if (grepl("mean", file_dir)) {
    x$DATA_FIELD <- "arithmetic_mean"
  } else {
    ## x$DATA_FIELD <- "NAAQS_statistic"
    x$DATA_FIELD <- map_standard_to_field(pollutant_standard)
  }
  if (grepl("event_included", file_dir)) {
    x$EVENT <- "Events Included"
  } else {
    x$EVENT <- "Events Excluded"
  }
  ## Tidyverse syntax: https://r-spatial.github.io/sf/reference/tidyverse.html
  x <- pivot_longer(x, X1997:X2023, names_to = "YEAR", values_to = "VALUE")
  x$YEAR <- as.integer(sub("^X", "", x$YEAR))
  dplyr::select(x, POLLUTANT_STANDARD:VALUE, everything())
}

## Summarize values by map
summarize_by_boundaries <- function(x, map, extensive = FALSE, ...) {
  if (!inherits(x, "stars")) {
    stop("'x' must be a stars object.")
  }
  if (!inherits(map, "sf")) {
    stop("'map' must be a sf object.")
  }
  map <- st_transform(map, st_crs(x))
  ## Cropping
  x <- st_as_sf(x[map])
  ## Areal-weighted interpolation
  suppressWarnings(x <- st_interpolate_aw(x, map, extensive = extensive, ...))
  st_join(map, x, join = st_equals)
}

## Helper function to organize a list of raster objects
pollutant_standard <- c(
  "CO_1_hour_1971", "CO_8_hour_1971", "SO2_1_hour_2010", "NO2_1_hour_2010",
  "NO2_annual_1971", "Ozone_8_hour_2015", "PM10_24_hour_2006",
  "PM2.5_24_hour_2012", "PM2.5_annual_2012"
)

get_grid <- function(x, pollutant_standard) {
  x <- lapply(x, function(x) x$grid)
  idx <- grep(pollutant_standard, names(x))
  x <- x[idx]
  idx_event_included <- grep("included", names(x))
  idx_event_excluded <- grep("excluded", names(x))
  x_event_included <- do.call(c, c(x[idx_event_included], along = "data_field"))
  x_event_excluded <- do.call(c, c(x[idx_event_excluded], along = "data_field"))
  c(x_event_included, x_event_excluded, along = "event")
}

get_summarized <- function(x, type = c("county", "census_tract",
                                       "block_group", "zip_code")) {
  type <- match.arg(type)
  bind_rows(lapply(x, function(y) y[[type]])) # sf object
}

## Find all TIFF files
file_list <- list.files(
  ## Images created using raqs and pargasite packages
  path = "~/Dropbox/GitHub/pollutant_grid/TIFF",
  pattern = ".tiff$",
  recursive = TRUE,
  full.names = TRUE
)

## Split files by sub directories
file_list_by_sub <- split(file_list, dirname(file_list))

## Joining data for each sub-directory
## Do sequentially due to memory demand
airquality_data <- list()
for (i in names(file_list_by_sub)) {
  ## read image files; list object
  conus_grid <- lapply(
    file_list_by_sub[[i]],
    function(x) setNames(read_stars(x), "value")
  )
  ## combine them along year dim
  conus_grid <- do.call(c, c(conus_grid, along = "band"))
  ## verify year values = 1997:2023
  stopifnot(setequal(st_get_dimension_values(conus_grid, "band"), 1997:2023))
  ## crop and summarize
  dat_grid <- conus_grid[st_transform(.maps$cbsa, st_crs(conus_grid))]
  dat_county <- summarize_by_boundaries(conus_grid, .maps$county)
  dat_tract <- summarize_by_boundaries(conus_grid, .maps$census_tract)
  ## dat_block <- summarize_by_boundaries(conus_grid, .maps$block_group)
  dat_zipcode <- summarize_by_boundaries(conus_grid, .maps$zip_code)
  file_dir <- names(file_list_by_sub[i])
  file_dir <- sub("(.*)/TIFF/(.*)", "\\2", file_dir)
  out_name <- gsub("/", "_", file_dir)
  airquality_data[[out_name]] <- list(
    grid = format_grid(dat_grid, file_dir),
    county = format_aw(dat_county, file_dir),
    census_tract = format_aw(dat_tract, file_dir),
    ## block_group = format_aw(dat_block, file_dir),
    zip_code = format_aw(dat_zipcode, file_dir)
  )
  rm(conus_grid)
}

airquality_grid <- lapply(pollutant_standard, \(x) get_grid(airquality_data, x))
airquality_grid <- setNames(airquality_grid, pollutant_standard)

airquality_county <- get_summarized(airquality_data, "county")
airquality_tract <- get_summarized(airquality_data, "census_tract")
## airquality_block <- get_summarized(airquality_data, "block_group")
## airquality_zipcode <- get_summarized(airquality_data, "zip_code")

airquality_summary <- list(
  ## grid = lapply(airquality_grid, \(x) st_warp(x, crs = 3857)),
  county = st_transform(airquality_county, 4326),
  census_tract = st_transform(airquality_tract, 4326)
  ## block_group = st_transform(airquality_block, 4326),
  ## zip_code = st_transform(airquality_zipcode, 4326)
)

saveRDS(
  airquality_summary,
  file.path("sapphirine_data", "RDS", "epa_airquality.RDS"),
  compress = FALSE
)
