#' Zoom function
#'
#' Helper function for `localMap` and `EPAMap`. Gives optimal zoom for initial display in leaflet maps
#'
#' @param x The range of x coordinates (longitudes) of the display
#' @param y The range of y coordinates (latitudes)
#' @export

f.zoom <- function(x, y){
  val <- ifelse(x > y, x, y)
  return(as.integer(round(11.47 - 1.5*val, digits = 0)))
}
