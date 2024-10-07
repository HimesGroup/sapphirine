library(sf)
library(stars)
library(dplyr)
library(tidyr)

## map data
.maps <- readRDS(file.path("sapphirine_data", "RDS", "map.RDS"))

pa_files <- list.files(
  file.path("sapphirine_data", "Traffic", "PA"),
  pattern = "shp$", full.names = TRUE, recursive = TRUE
)

philly_map <- .maps$census_tract %>%
  filter(NAMELSADCO == "Philadelphia County")

philly_traffic <- lapply(pa_files, \(x) {
  year <- sub("(.*)(\\d{4})(.*)", "\\2", dirname(x))
  d <- st_read(x) %>%
    mutate(YEAR = as.integer(year)) %>%
    st_transform(st_crs(philly_map)) %>%
    rename(AADT = CUR_AADT) %>%
    select(AADT, YEAR) %>%
    ## rename(VALUE = AADT) %>%
    .[philly_map, ]
}) %>%
  bind_rows() %>%
  st_transform(32618) %>%
  mutate(DVMT = as.numeric(AADT * st_length(.) * 0.000621371)) %>%
  st_transform(st_crs(philly_map))

saveRDS(
  philly_traffic,
  file.path("sapphirine_data", "RDS", "traffic.RDS"),
  compress = FALSE
)


