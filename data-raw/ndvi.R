library(sf)
library(stars)
library(dplyr)
library(tidyr)

## Map
.maps <- readRDS(file.path("sapphirine_data", "RDS", "map.RDS"))

## NVDI files from Google Earth
file_ndvi <- list.files(
  file.path("sapphirine_data", "NDVI"),
  pattern = ".tif$", full.names = TRUE
)

read_ndvi <- function(x) {
  obj <- setNames(read_stars(x), "value")
  year <- sub("^NDVI(\\d+)\\.tif$", "\\1", basename(x))
  c(obj, along = list(YEAR = year))
}

ndvi_data <- lapply(file_ndvi, read_ndvi)

ndvi_grid <- lapply(ndvi_data, \(x) {
  map <- st_transform(.maps$cbsa, st_crs(x))
  x[map]
})
ndvi_grid <- do.call(c, c(ndvi_grid, along = "YEAR"))

aw_by <- function(x, map, extensive = FALSE, ...) {
  map <- st_transform(map, st_crs(x))
  x <- st_as_sf(x[map])
  suppressWarnings(x <- st_interpolate_aw(x, map, extensive = extensive,
                                          na.rm = TRUE, ...))
  x <- st_join(map, x, join = st_equals)
  x <- rename_with(x, ~sub("^X(\\d+)$", "\\1", .x))
  col_to_pick <- grep("^\\d+$", names(x), value = TRUE)
  pivot_longer(x, cols = any_of(col_to_pick),
               names_to = "YEAR", values_to = "VALUE") %>%
    mutate(YEAR = as.integer(YEAR))
}

ndvi_county <- aw_by(ndvi_grid, .maps$county)

ndvi_tract <- aw_by(ndvi_grid, .maps$census_tract)

## ndvi_zipcode <- lapply(ndvi_data, \(x) aw_by(x, .maps$zip_code)) %>%
##   bind_rows()

ndvi_summary <- list(
  ## grid = ndvi_grid,
  county = ndvi_county,
  census_tract = ndvi_tract
  ## zip_code = ndvi_zipcode
)

saveRDS(
  ndvi_summary,
  file.path("sapphirine_data", "RDS", "ndvi.RDS"),
  compress = FALSE
)
