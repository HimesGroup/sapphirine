#' Select Greater Philadelphia counties
#'
#' Allows for easy selection of Greater Philadelphia Area counties to generate a custom shapefile.
#'
#' @param counties A vector containing names of Greater Philadelphia counties to be included. Proper names can be accessed from `sapphirine::GPACountyNames`
#' @return A SpatialPolygonsDataFrame shapefile of the input counties.
#' @import sp
#' @export
#' @examples
#' #Print a list of county names for reference
#' GPACountyNames
#'
#' counties <- c('Bucks', 'Chester', 'Delaware', 'Montgomery', 'Philadelphia')
#'
#' selectGPACounties(counties)

selectGPACounties <- function(counties){

  for(i in 1:length(counties)){

    if(!counties[i] %in% GPACountyNames){
      stop('counties must be a vector of county names included in GPACountyNames')
    }

    shp.layer <- GPA_counties[GPA_counties$NAME == counties[i],]

    if(i == 1){
      newShape <- shp.layer
    } else {
      newShape <- rbind(newShape, shp.layer)
    }

  }

  return(newShape)

}
