library(sf)
library(dplyr)
library(tidyr)

## map data
.maps <- readRDS(file.path("sapphirine_data", "RDS", "map.RDS"))

crime_files <- list.files(
  file.path("sapphirine_data", "Crime"), pattern = "shp$",
  recursive = TRUE, full.names = TRUE
)

crime_data <- lapply(crime_files, function(x) {
  year <- sub("(.*)(\\d{4})(.*)", "\\2", x)
  d <- st_read(x)
  crime_dummy <- data.frame(model.matrix(~ 0 + text_gener, data = d))
  names(crime_dummy) <- sub("^text_gener", "", names(crime_dummy))
  d <- cbind(d, crime_dummy) %>%
    filter(!st_is_empty(.)) %>% # remove empty geometry
    mutate(YEAR = year)
  ag_list <- list()
  for (i in names(crime_dummy)) {
    ag_list[[i]] <- aggregate(d[i], .maps$census_tract, FUN = \(y) sum(y, na.rm = TRUE))
  }
  census_tract <- Reduce(function(x, y) st_join(x, y, st_equals), ag_list) %>%
    st_join(x = .maps$census_tract, y = ., st_equals) %>%
    filter(NAMELSADCO == "Philadelphia County") %>%
    mutate(across(all_of(names(crime_dummy)), \(x) replace_na(x, 0))) %>%
    rowwise() %>%
    mutate(Total = sum(c_across(names(crime_dummy)))) %>%
    ungroup() %>%
    mutate(YEAR = year)
  list(location = d[, c("YEAR", "text_gener")], census_tract = census_tract)
})

crime_location <- lapply(crime_data, \(x) x$location) %>%
  bind_rows()

crime_tract <- lapply(crime_data, \(x) x$census_tract) %>%
  bind_rows()

crime_summary <- list(
  location = crime_location,
  census_tract = crime_tract
)

## Remember that region-based filtering removes crimes with unknown locations
saveRDS(
  crime_summary,
  file.path("sapphirine_data", "RDS", "crime.RDS"),
  compress = FALSE
)

