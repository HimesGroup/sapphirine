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
  filter(grepl("Philadelphia County", LOCATION))

philly_traffic <- lapply(pa_files, \(x) {
  year <- sub("(.*)(\\d{4})(.*)", "\\2", dirname(x))
  d <- st_read(x) %>%
    st_transform(st_crs(philly_map)) %>%
    rename(AADT = CUR_AADT, DVMT = DLY_VMT) %>%
    mutate(
      ## Re-conttruct Segment Key (23) due to missingness
      ## Concatenation of CTY_CODE (2), ST_RT_NO (4), SEG_PT_BGN (8), SEG_PT_END (8), and JURIS (1)
      ## SEG_PT_BGN: concatenated SEG_BGN + OFFSET_BGN
      ## SEG_PT_END: concatenated SEG_END + OFFSET_END
      RMSTRAFFIC_LRS_KEY = paste0(
        CTY_CODE, ST_RT_NO, SEG_BGN, sprintf("%04d", OFFSET_BGN),
        SEG_END, sprintf("%04d", OFFSET_END), JURIS
      ),
      YEAR = as.integer(year)
    ) %>%
  select(RMSTRAFFIC_LRS_KEY, YEAR, AADT, DVMT) %>%
  .[philly_map, ]
}) %>%
  bind_rows()

saveRDS(
  philly_traffic,
  file.path("sapphirine_data", "RDS", "traffic.RDS"),
  compress = FALSE
)

