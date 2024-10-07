library(sf)
library(dplyr)
library(tidyr)
library(tidycensus)

## Map
.maps <- readRDS(file.path("sapphirine_data", "RDS", "map.RDS"))

violation_files <- list.files(
  file.path("sapphirine_data", "Violations"),
  pattern = "shp$",
  recursive = TRUE, full.names = TRUE
)

water_damage <- c(
  "14-704/1",  "14-704/2", "A-302.1/1", "CP-303","CP-304", "CP-342",
  "CP-349", "PM-302.4/1", "PM-302.4/5", "PM-304.4/1", "PM-304.8/6",
  "PM15-302.2", "PM15-304.16", "PM15-304.1D", "PM15-304.2", "PM15-304.6",
  "PM15-304.7"
)

air_contaminant <- c("03-306/1", "03-306/2")

pest_infestation <- c(
  "CP-306", "CP-307", "CP-308", "CP-327", "CP-336", "CP-337", "CP-349",
  "PM-303.4/1", "PM-303.4/2", "PM-303.5/1", "PM-303.5/2", "PM-303.5/3",
  "PM-303.5/4", "PM-304.2/1", "PM-304.2/2", "PM-304.8/19", "PM-304.8/20",
  "PM-304.8/6", "PM-305.3/1", "PM15-302.5", "PM15-304.16", "PM15-304.17",
  "PM15-304.5", "PM15-309.1", "PM15-309.3", "PM15-309.4"
)

violation_data <- lapply(violation_files, \(x) {
  d <- filter(
    st_read(x), violationc %in% c(water_damage, air_contaminant, pest_infestation)
  )
  d[.maps$cbsa, ]
}) %>%
  bind_rows() %>%
  ## one record per one incident
  mutate(
    YEAR = sub("(^\\d{4})-(.*)", "\\1", violationd),
    water_damage = if_else(violationc %in% water_damage, 1L, 0L),
    air_contaminant = if_else(violationc %in% air_contaminant, 1L, 0L),
    pest_infestation = if_else(violationc %in% pest_infestation, 1L, 0L)
  ) %>%
  filter(YEAR != 2024) %>% # incomplete data for present year
  split(f = .[["YEAR"]])

sum_by <- function(year, geography = c("tract", "block group")) {
  geography < match.arg(geography)
  lapply(year, \(k) {
    x <- violation_data[[as.character(k)]]
    target_map <- get_acs(geography = geography,
                          variables = "B01001_001",
                          state = "PA",
                          year = k,
                          county = "Philadelphia",
                          geometry = TRUE) %>%
      st_transform(st_crs(x)) %>%
      mutate(YEAR = k)
    water_damage <- aggregate(x["water_damage"], target_map, \(y) sum(y, na.rm = TRUE))
    air_contaminant <- aggregate(x["air_contaminant"], target_map, \(y) sum(y, na.rm = TRUE))
    pest_infestation <- aggregate(x["pest_infestation"], target_map, \(y) sum(y, na.rm = TRUE))
    st_join(target_map, water_damage, st_equals) |>
      st_join(air_contaminant, st_equals) |>
      st_join(pest_infestation, st_equals) |>
      ## filter(NAMELSADCO == "Philadelphia County") |>
      mutate(across(c(water_damage, air_contaminant, pest_infestation), \(y) replace_na(y, 0))) |>
      mutate(total = water_damage + air_contaminant + pest_infestation)
  }) %>%
    bind_rows() %>%
    mutate(NAME = gsub(";", ",", sub("Pennsylvania$", "PA", NAME)))
}

violation_location <- violation_data %>%
  bind_rows(.id = "YEAR") %>%
  select(violationc, YEAR, violatio_1) %>%
  filter(YEAR %in% 2013:2022)
violation_tract <- sum_by(2013:2022, "tract")
violation_block <- sum_by(2013:2022, "block group")

violation_summary <- list(
  location = violation_location,
  census_tract = violation_tract,
  block_group = violation_block
)

saveRDS(
  violation_summary,
  file.path("sapphirine_data", "RDS", "violation.RDS"),
  compress = FALSE
)
