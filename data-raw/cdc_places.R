library(sf)
library(dplyr)
library(CDCPLACES)

## map data
.maps <- readRDS(file.path("sapphirine_data", "RDS", "map.RDS"))

get_census <- function(release) {
  d_pa <- get_places(geography = "census", state = "PA", release = release)
  d_nj <- get_places(geography = "census", state = "NJ", release = release)
  d_de <- get_places(geography = "census", state = "DE", release = release)
  d_md <- get_places(geography = "census", state = "MD", release = release)
  rbind(d_pa, d_nj, d_de, d_md) %>%
    rename(GEOID = locationid) %>%
    mutate(release = release) %>%
    select(release, everything()) %>%
    merge(.maps$census_tract, .)
}

## d2020_census <- get_census(2020)
## d2021_census <- get_census(2021)
## d2022_census <- get_census(2022)
## d2023_census <- get_census(2023)
d2024_census <- get_census(2024)

d_census <- rbind(
  ## d2020_census, d2021_census, d2022_census, d2023_census,
  ## select(d2024_census, -totalpop18plus)
  d2024_census
)


get_county <- function(release) {
  d_pa <- get_places(geography = "county", state = "PA", release = release, geometry = TRUE)
  d_nj <- get_places(geography = "county", state = "NJ", release = release, geometry = TRUE)
  d_de <- get_places(geography = "county", state = "DE", release = release, geometry = TRUE)
  d_md <- get_places(geography = "county", state = "MD", release = release, geometry = TRUE)
  rbind(d_pa, d_nj, d_de, d_md) %>%
    rename(GEOID = locationid) %>%
    mutate(release = release) %>%
    select(release, everything()) %>%
    st_drop_geometry() %>%
    merge(.maps$county, .)
}

## d2020_county <- get_county(2020)
## d2021_county <- get_county(2021)
## d2022_county <- get_county(2022)
## d2023_county <- get_county(2023)
d2024_county <- get_county(2024)

d_county <- rbind(
  ## select(d2020_county, -c(locationname_p, latitude)),
  ## d2021_county, d2022_county, d2023_county,
  ## select(d2024_county, -totalpop18plus)
  d2024_county
)

cdc_places_summary <- list(
  county = d_county,
  census_tract = d_census
)

saveRDS(
  cdc_places_summary,
  file.path("sapphirine_data", "RDS", "cdc_places.RDS"),
  compress = FALSE
)

