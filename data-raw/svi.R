library(sf)
library(dplyr)
library(tidyr)

## Map
.maps <- readRDS(file.path("sapphirine_data", "RDS", "map.RDS"))

## TL shape
svi_tl_2000_files <- list.files(
  file.path("sapphirine_data", "SVI", "TL", "2000"),
  pattern = "shp$", full.names = TRUE, recursive = TRUE
)
svi_tl_2000 <- bind_rows(lapply(svi_tl_2000_files, st_read))

svi_tl_2010_files <- list.files(
  file.path("sapphirine_data", "SVI", "TL", "2010"),
  pattern = "shp$", full.names = TRUE, recursive = TRUE
)
svi_tl_2010 <- bind_rows(lapply(svi_tl_2010_files, st_read))

svi_tl_2014_files <- list.files(
  file.path("sapphirine_data", "SVI", "TL", "2014"),
  pattern = "shp$", full.names = TRUE, recursive = TRUE
)
svi_tl_2014 <- bind_rows(lapply(svi_tl_2014_files, st_read))

## Census tract
svi_tract_files <- list.files(
  file.path("sapphirine_data", "SVI", "census_tract"), full.names = TRUE
)

svi_tract_list <- lapply(svi_tract_files, \(x) {
  st_read(
    x, wkt_filter = st_as_text(st_as_sfc(st_bbox(st_transform(.maps$cbsa, 4269))))
  )
})
svi_tract_list <- setNames(svi_tract_list, gsub("\\D+", "", svi_tract_files))

svi_tract_cols <- c("STATE", "ST_ABBR", "COUNTY", "FIPS", "LOCATION",
                    "RPL_THEME1", "RPL_THEME2", "RPL_THEME3", "RPL_THEME4",
                    "RPL_THEMES")
svi_tract_list[["2010"]] <- rename(
  svi_tract_list[["2010"]],
  STATE = STATE_NAME, ST_ABBR = STATE_ABBR, RPL_THEME1 = R_PL_THEME1,
  RPL_THEME2 = R_PL_THEME2, RPL_THEME3 = R_PL_THEME3, RPL_THEME4 = R_PL_THEME4,
  RPL_THEMES = R_PL_THEMES
)

svi_tract_summary <- lapply(svi_tract_list[-1], \(x) x[, svi_tract_cols]) |>
  bind_rows(.id = "YEAR") |>
  mutate(across(all_of(contains("RPL_THEME")), ~na_if(., -999))) |>
  mutate(LOCATION = na_if(LOCATION, "-999")) |>
  mutate(LOCATION = sub("(.*),(.*),(.*)", "\\1, \\2", LOCATION)) |>
  mutate(LOCATION = if_else(is.na(LOCATION), NA_character_, paste0(LOCATION, ", ", ST_ABBR))) |>
  st_transform(4326)

## County
svi_county_files <- list.files(
  file.path("sapphirine_data", "SVI", "county"), full.names = TRUE
)

svi_county_list <- lapply(svi_county_files, function(x) {
  st_read(x, wkt_filter = st_as_text(st_as_sfc(st_bbox(st_transform(.maps$cbsa, 4269)))))
})
svi_county_list <- setNames(svi_county_list, gsub("\\D+", "", svi_county_files))

svi_county_cols <- setdiff(svi_tract_cols, "LOCATION")
svi_county_list[["2010"]] <- rename(
  svi_county_list[["2010"]],
  ST_ABBR = ST, RPL_THEME1 = R_PL_THEME1, RPL_THEME2 = R_PL_THEME2,
  RPL_THEME3 = R_PL_THEME3, RPL_THEME4 = R_PL_THEME4, RPL_THEMES = R_PL_THEMES
) |>
  mutate(COUNTY = sub("(.*),(.*)", "\\1", LOCATION))

svi_county_summary <- lapply(svi_county_list[-1], function(x) x[, svi_county_cols]) |>
  bind_rows(.id = "YEAR") |>
  st_transform(4326) |>
  mutate(LOCATION = paste0(COUNTY, ", ", ST_ABBR)) |>
  mutate(across(all_of(contains("RPL_THEME")), ~na_if(., -999))) |>
  mutate(LOCATION = na_if(LOCATION, "-999")) |>
  select(YEAR:FIPS, LOCATION, everything())


svi_county_summary <- .maps$county |>
  mutate(FIPS = paste0(STATEFP, COUNTYFP)) |>
  select(FIPS) |>
  merge(st_drop_geometry(svi_county_summary))

svi_tract_summary <- .maps$census_tract |>
  rename(FIPS = GEOID) |>
  select(FIPS) |>
  merge(st_drop_geometry(svi_tract_summary))

svi_summary <- list(
  county = svi_county_summary,
  census_tract = svi_tract_summary
)

saveRDS(
  svi_summary,
  file.path("sapphirine_data", "RDS", "svi.RDS"),
  compress = FALSE
)
