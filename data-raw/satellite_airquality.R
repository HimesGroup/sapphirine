library(sf)
library(ncmeta)
library(stars)
library(terra)
library(dplyr)
library(tidyr)
library(units)

## Map
.maps <- readRDS(file.path("sapphirine_data", "RDS", "map.RDS"))

#################################################################################
## Cooper 2022
#################################################################################
file_cooper <- list.files(
  file.path("sapphirine_data", "Satellite_no2", "Cooper2022"),
  pattern = ".nc$", full.names = TRUE
)

pollutant_cooper <- lapply(file_cooper, \(x) {
  no2 <- read_ncdf(x, var = "surface_no2_ppb")
  lon <- read_ncdf(x, var = "LON_CENTER")
  lat <- read_ncdf(x, var = "LAT_CENTER")
  year <- sub("(.*)_(\\d+)(.nc$)", "\\2", basename(x))
  obj <- st_as_stars(
    list(value = no2[[1]]),
    dimensions = st_dimensions(x = as.vector(lon[[1]]), y = as.vector(lat[[1]])),
  )
  obj <- c(obj, along = list(year = year))
  st_set_crs(obj, 4326) %>%
    st_warp(crs = 3857)
})

pollutant_cooper_grid <- lapply(pollutant_cooper, \(x) {
  map <- st_transform(.maps$cbsa, st_crs(x))
  x[map]
})
pollutant_cooper_grid <- do.call(c, c(pollutant_cooper_grid, along = "year"))

aw_by <- function(x, map, extensive = FALSE, ...) {
  map <- st_transform(map, st_crs(x))
  x <- st_as_sf(x[map])
  suppressWarnings(
    x <- st_interpolate_aw(x, map, extensive = extensive, na.rm = TRUE, ...)
  )
  x <- st_join(map, x, join = st_equals)
  x <- rename_with(x, ~sub("^X(\\d+)$", "\\1", .x))
  col_to_pick <- grep("^\\d+$", names(x), value = TRUE)
  pivot_longer(x, cols = any_of(col_to_pick), names_to = "year") %>%
    mutate(year = as.integer(year))
}

pollutant_cooper_county <- aw_by(pollutant_cooper_grid, .maps$county)

pollutant_cooper_tract <- aw_by(pollutant_cooper_grid, .maps$census_tract)

## pollutant_cooper_block <- aw_by(pollutant_cooper_grid, .maps$block_group)

## pollutant_cooper_zipcode <- lapply(
##   pollutant_cooper, \(x) aw_by(x, .maps$zip_code)
## ) %>%
##   bind_rows()

cooper_summary <- list(
  ## grid = pollutant_cooper_grid,
  county = pollutant_cooper_county,
  census_tract = pollutant_cooper_tract
  ## block_group = pollutant_cooper_block,
  ## zip_code = pollutant_cooper_zipcode
)

rm(pollutant_cooper, pollutant_cooper_grid, pollutant_cooper_county,
   pollutant_cooper_tract, pollutant_cooper_zipcode)

#################################################################################
## Annenberg 2022
#################################################################################
file_annenberg <- list.files(
  file.path("sapphirine_data", "Satellite_no2", "Annenberg2022"),
  pattern = ".tif$", full.names = TRUE
)

pollutant_annenberg <- lapply(file_annenberg, \(x) {
  obj <- setNames(read_stars(x, proxy = FALSE), "value")
  bounding <- st_bbox(c(xmin = -76.3, xmax = -74.2, ymin = 39.15, ymax = 40.75),
                      crs = st_crs(obj))
  obj <- st_crop(obj, bounding)
  year <- sub("^(\\d+)_(.*)", "\\1", basename(x))
  c(obj, along = list(year = year)) %>%
    st_warp(crs = 3857)
})

pollutant_annenberg_grid <- lapply(pollutant_annenberg, \(x) {
  map <- st_transform(.maps$cbsa, st_crs(x))
  x[map]
})
pollutant_annenberg_grid <- do.call(c, c(pollutant_annenberg_grid, along = "year"))

pollutant_annenberg_county <- aw_by(pollutant_annenberg_grid, .maps$county)

pollutant_annenberg_tract <- aw_by(pollutant_annenberg_grid, .maps$census_tract)

## pollutant_annenberg_block <- aw_by(pollutant_annenberg_grid, .maps$block_group)

## pollutant_annenberg_zipcode <- lapply(
##   pollutant_annenberg, \(x) aw_by(x, .maps$zip_code)
## ) %>%
##   bind_rows()

annenberg_summary <- list(
  ## grid = pollutant_annenberg_grid,
  county = pollutant_annenberg_county,
  census_tract = pollutant_annenberg_tract
  ## block_group = pollutant_annenberg_block,
  ## zip_code = pollutant_annenberg_zipcode
)

rm(pollutant_annenberg, pollutant_annenberg_grid, pollutant_annenberg_county,
   pollutant_annenberg_tract, pollutant_annenberg_zipcode)

#################################################################################
## Shen 2024
#################################################################################
file_shen <- list.files(
  file.path("sapphirine_data", "Satellite_pm25", "Shen2024"),
  pattern = ".nc$", full.names = TRUE
)

pollutant_shen <- lapply(file_shen, \(x) {
  obj <- terra::rast(x)
  bounding <- st_bbox(c(xmin = -76.3, xmax = -74.2, ymin = 39.15, ymax = 40.75),
                      crs = st_crs(obj))
  year <- sub("(.*)-(\\d+)(12.nc$)", "\\2", basename(x))
  obj <- crop(obj, bounding) %>%
    st_as_stars() %>%
    st_set_crs(4326) %>%
    st_warp(crs = 3857) %>%
    setNames(nm = "value")
  obj <- c(obj, along = list(year = year))
})

pollutant_shen_grid <- lapply(pollutant_shen, \(x) {
  map <- st_transform(.maps$cbsa, st_crs(x))
  x[map]
})
pollutant_shen_grid <- do.call(c, c(pollutant_shen_grid, along = "year"))

aw_by <- function(x, map, extensive = FALSE, ...) {
  map <- st_transform(map, st_crs(x))
  x <- st_as_sf(x[map])
  suppressWarnings(
    x <- st_interpolate_aw(x, map, extensive = extensive, na.rm = TRUE, ...)
  )
  x <- st_join(map, x, join = st_equals)
  x <- rename_with(x, ~sub("^X(\\d+)$", "\\1", .x))
  col_to_pick <- grep("^\\d+$", names(x), value = TRUE)
  pivot_longer(x, cols = any_of(col_to_pick), names_to = "year") %>%
    mutate(year = as.integer(year))
}

pollutant_shen_county <- aw_by(pollutant_shen_grid, .maps$county)

pollutant_shen_tract <- aw_by(pollutant_shen_grid, .maps$census_tract)

## pollutant_shen_block <- aw_by(pollutant_shen_grid, .maps$block_group)

## pollutant_shen_zipcode <- lapply(
##   pollutant_shen, \(x) aw_by(x, .maps$zip_code)
## ) %>%
##   bind_rows()

shen_summary <- list(
  ## grid = pollutant_shen_grid,
  county = pollutant_shen_county,
  census_tract = pollutant_shen_tract
  ## block_group = pollutant_shen_block,
  ## zip_code = pollutant_shen_zipcode
)

rm(pollutant_shen, pollutant_shen_grid, pollutant_shen_county,
   pollutant_shen_tract, pollutant_shen_zipcode)

#################################################################################
## van Donkelaar 2021
#################################################################################
file_van <- list.files(
  file.path("sapphirine_data", "Satellite_pm25", "van_Donkelaar2021"),
  pattern = ".nc$", full.names = TRUE
)

pollutant_van <- lapply(file_van, \(x) {
  obj <- terra::rast(x)
  bounding <- st_bbox(c(xmin = -76.3, xmax = -74.2, ymin = 39.15, ymax = 40.75),
                      crs = st_crs(obj))
  year <- sub("(.*)-(\\d+)(12.nc$)", "\\2", basename(x))
  obj <- crop(obj, bounding) %>%
    st_as_stars() %>%
    st_set_crs(4326) %>%
    st_warp(crs = 3857) %>%
    setNames(nm = "value")
  obj <- c(obj, along = list(year = year))
})

pollutant_van_grid <- lapply(pollutant_van, \(x) {
  map <- st_transform(.maps$cbsa, st_crs(x))
  x[map]
})
pollutant_van_grid <- do.call(c, c(pollutant_van_grid, along = "year"))

pollutant_van_county <- aw_by(pollutant_van_grid, .maps$county)

pollutant_van_tract <- aw_by(pollutant_van_grid, .maps$census_tract)

## pollutant_van_block <- aw_by(pollutant_van_grid, .maps$block_group)

## pollutant_van_zipcode <- lapply(
##   pollutant_van, \(x) aw_by(x, .maps$zip_code)
## ) %>%
##   bind_rows()

van_summary <- list(
  ## grid = pollutant_van_grid,
  county = pollutant_van_county,
  census_tract = pollutant_van_tract
  ## block_group = pollutant_van_block,
  ## zip_code = pollutant_van_zipcode
)

rm(pollutant_van, pollutant_van_grid, pollutant_van_county,
   pollutant_van_tract, pollutant_van_zipcode)

#################################################################################

satellite_summary <- list(
  no2_cooper = cooper_summary,
  no2_annenberg = annenberg_summary,
  pm25_shen = shen_summary,
  pm25_van = van_summary
)

saveRDS(
  satellite_summary,
  file.path("sapphirine_data", "RDS", "satellite_airquality.RDS"),
  compress = FALSE
)
