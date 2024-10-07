library(sf)
library(dplyr)

## map data
.maps <- readRDS(file.path("sapphirine_data", "RDS", "map.RDS"))

#################################################################################
## ADI from neighborhood atlas
#################################################################################
adi_files <- list.files(
  file.path("sapphirine_data", "ADI"), pattern = "csv$",
  recursive = TRUE, full.names = TRUE
)
adi_list <- lapply(adi_files[2:3], function(x) {
  year <- sub("(.*)(\\d{4})(.*)", "\\2", x)
  d <- read.csv(x, sep = ",")
  d$YEAR <- year
  d <- d[, c("FIPS", "ADI_NATRANK", "ADI_STATERNK", "YEAR")]
  d <- setNames(d, c("GEOID", "ADI_NATRANK", "ADI_STATERNK", "YEAR"))
  county_nms <- unique(
    st_drop_geometry(.maps$county)[, c("STATEFP", "COUNTYFP", "NAMELSAD", "STUSPS")]
  )
  names(county_nms)[3] <- "COUNTY"
  m <- merge(.maps$block_group[, c("GEOID", "STATEFP", "COUNTYFP", "NAMELSAD")],
             county_nms)
  m$label <- paste(m$NAMELSAD, m$COUNTY, m$STUSPS, sep = ", ")
  merge(m[, c("GEOID", "label")], d)
})

adi <- do.call(rbind, adi_list) # need to figure out issue in 2015 (non-unique FIPS)
saveRDS(adi, file.path("sapphirine_data", "RDS", "adi.RDS"), compress = FALSE)
