library(sf)
library(dplyr)

## Map
.maps <- readRDS(file.path("sapphirine_data", "RDS", "map.RDS"))

## TRI files
file_tri <- list.files(
  file.path("sapphirine_data", "TRI"),
  pattern = ".csv$", full.names = TRUE
)

read_tri <- function(x) {
  d <- read.csv(x)
  d <- setNames(d, sub("^X(.*)\\.\\.([A-Z]+)(.*)", "\\2\\3", names(d)))
  d <- st_as_sf(d, coords = c("LONGITUDE", "LATITUDE"), crs = 4269)
  d <- st_transform(d, 4326)
  cols_to_get <- c("YEAR", "FACILITY.NAME", "UNIT.OF.MEASURE", "FUGITIVE.AIR", "STACK.AIR")
  d <- d[, cols_to_get]
  bounding <- st_bbox(c(xmin = -76.3, xmax = -74.2, ymin = 39.15, ymax = 40.75),
                      crs = st_crs(d))
  d <- st_crop(d, bounding)
  ## d <- d[.maps$cbsa, ] # Don't do if zip-code level summary needs to be calculated
  stopifnot(!anyNA(d$UNIT.OF.MEASURE))
  ## Unit to pound
  d$FUGITIVE.AIR <- with(
    d,
    ifelse(UNIT.OF.MEASURE == "Grams", FUGITIVE.AIR * 0.00220462, FUGITIVE.AIR)
  )
  d$STACK.AIR <- with(
    d,
    ifelse(UNIT.OF.MEASURE == "Grams", STACK.AIR * 0.00220462, STACK.AIR)
  )
  d[, setdiff(names(d), "UNIT.OF.MEASURE")]
}

tri_data <- lapply(file_tri, read_tri) %>%
  bind_rows()

summarize_by <- function(x, by) {
  lapply(split(x, x[["YEAR"]]), \(y) {
    fugitive <- aggregate(y["FUGITIVE.AIR"], by = by, FUN = sum)
    stack <- aggregate(y["STACK.AIR"], by = by, FUN = sum)
    st_join(fugitive, stack, join = st_equals)
  }) %>%
    bind_rows(.id = "YEAR") %>%
    st_join(x = by, y = ., join = st_equals) %>%
    mutate(YEAR = as.integer(YEAR))
}

tri_county <- summarize_by(tri_data, .maps$county)
tri_tract <- summarize_by(tri_data, .maps$census_tract)
## tri_zipcode <- summarize_by(tri_data, .maps$zip_code)
## tri_block <- summarize_by(tri_data, .maps$block_group)

tri_summary <- list(
  location = tri_data[.maps$cbsa, ],
  county = tri_county,
  census_tract = tri_tract
  ## block_group = tri_block
  ## zip_code = tri_zipcode
)

saveRDS(
  tri_summary,
  file.path("sapphirine_data", "RDS", "tri.RDS"),
  compress = FALSE
)
