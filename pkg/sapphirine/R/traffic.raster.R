#' Import Traffic Raster
#'
#' @import raster
#' @import usethis

traffic.raster <- raster('data/traffic.raster.tif')
use_data(traffic.raster, overwrite = T)
