## Philadelphia-Camden-Wilmington statistical area (code: 37980)
library(sf)
library(dplyr)

#################################################################################
## Shape for Philadelphia-Camden-Wilmington statistical area (code: 37980)
#################################################################################
get_map_url <- function(x) {
  switch(
    x,
    "state" = "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_state_500k.zip",
    "county" = "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_county_500k.zip",
    "cbsa" = "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_cbsa_500k.zip",
    "tract" = "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_tract_500k.zip",
    "block" = "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_bg_500k.zip",
    "zipcode" = "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_zcta520_500k.zip"
  )
}

get_shape_file <- function(boundary = c("state", "county", "cbsa",
                                         "tract", "block", "zipcode"),
                            quiet = TRUE, force = FALSE, ...) {
  boundary <- match.arg(boundary)
  url <- get_map_url(boundary)
  ## file_dir <- getOption("sapphirine.shape_dir")
  file_dir <- tempdir()
  dir.create(file_dir, showWarnings = FALSE)
  file_path <- file.path(file_dir, basename(url))
  if (!file.exists(file_path) || force) {
    message("Processing US shape file...")
    download.file(url, file_path, mode = "wb", quiet = FALSE)
    unzip(file_path, exdir = file_dir, overwrite = TRUE)
  }
  shape_file <- sub("\\.zip$", "", basename(url))
  sf::st_read(dsn = file_dir, layer = shape_file, quiet = quiet, ...)
}

map_cbsa_overlap <- function(x) {
  x <- st_transform(x, 4326)
  idx <- st_intersects(x, map_cbsa, sparse = FALSE) & !st_touches(x, map_cbsa, sparse = FALSE)
  x[idx, ]
}

us_cbsa <- get_shape_file("cbsa")
us_county <- get_shape_file("county")
us_tract <- get_shape_file("tract")
us_block <- get_shape_file("block")
us_zipcode <- get_shape_file("zipcode")

map_cbsa <- st_transform(us_cbsa[us_cbsa$GEOID == 37980, ], 4326) %>% # WGS84
  select(NAMELSAD) %>%
  rename(LOCATION = NAMELSAD)
map_county <- map_cbsa_overlap(us_county) %>%
  mutate(LOCATION = paste0(NAMELSAD, ", ", STUSPS)) %>%
  select(GEOID, LOCATION)
map_tract <- map_cbsa_overlap(us_tract) %>%
  mutate(LOCATION = paste0(NAMELSAD, ", ", NAMELSADCO, ", ", STUSPS)) %>%
  select(GEOID, LOCATION)
map_block <- map_cbsa_overlap(us_block) %>%
  inner_join(
    x = .,
    y = st_drop_geometry(us_tract) %>%
      mutate(NAMELSADTRACT = paste0(NAMELSAD, ", ", NAMELSADCO, ", ", STUSPS)) %>%
      select(STATEFP, COUNTYFP, TRACTCE, NAMELSADTRACT),
    by = c("STATEFP", "COUNTYFP", "TRACTCE")
  ) %>%
  mutate(LOCATION = paste0(NAMELSAD, ", ", NAMELSADTRACT)) %>%
  select(GEOID, LOCATION)  # need GEOID for ADI mapping
map_zipcode <- map_cbsa_overlap(us_zipcode) %>%
  rename(LOCATION = NAME20) %>%
  select(LOCATION)

.maps <- list(
  cbsa = map_cbsa,
  county = map_county,
  census_tract = map_tract,
  block_group = map_block,
  zip_code = map_zipcode
)

saveRDS(.maps, file.path("sapphirine_data", "RDS", "map.RDS"), compress = FALSE)
