#' Custom EPA Data
#'
#' Subset the sapphirine EPA data frame according to custom parameters
#'
#' @param shape A SpatialPolygons or extent object, for example, a subset of `sapphirine::GPA_counties` created using `sapphirine::selectGPACounties`, to define the geographic scope of data to include
#' @param startDate A `Date` object or `character` string of format yyyy-mm-dd defining the lower bound of the time frame over which to include data
#' @param endDate " " for the upper bound
#' @param variables A `character` vector of pollutant variables to include.
#' @import dplyr
#' @import magrittr
#' @import prevR
#' @import sp
#' @import lubridate
#' @export
#' @examples
#'
#' #Select counties for shape
#' counties <- c('Bucks', 'Chester', 'Delaware', 'Montgomery', 'Philadelphia')
#' cty.shape <- selectGPACounties(counties)
#'
#' #Retrieve local data in selected counties from 1 June 2017 - 31 May 2019
#' customEPAData(cty.shape, '2017-06-01', '2019-05-31')

customEPAData <- function(shape = sapphirine::GPA_counties, startDate, endDate,
                          variables = c(
                            'PM2.5', 'PM10', 'SO2', 'NO2', 'O3', 'CO')){

  if(class(shape) == 'Extent'){
    shape <- as(shape, 'SpatialPolygons')
    crs(shape) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  } else if(grepl('SpatialPolygons', class(shape))){
    crs(shape) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  } else if(!grepl('SpatialPolygons', class(shape))){
    stop('shape must be of class \"Extent,\" \"SpatialPolygons,\" or \"SpatialPolygonsDataFrame\"')
  }

  if(class(startDate) != 'Date') {startDate <- lubridate::date(startDate)}
  if(class(endDate) != 'Date') {endDate <- lubridate::date(endDate)}

  custom.data <- sapphirine::EPA.data %>%
    dplyr::filter(
      point.in.SpatialPolygons(Longitude, Latitude, shape),
      Date %in% startDate:endDate) %>%
    dplyr::select(Date, Longitude, Latitude, AQS_Site_ID, Local.Site.Name,
                  State.Name, City.Name,
                  Monitor_Start_Date, Last_Sample_Date,
                  starts_with(variables))

  if(nrow(custom.data) > 0){

    if(startDate < min(custom.data$Date)){
      warning(paste('Data are available beginning',
                    min(custom.data$Date), 'given user-defined parameters.'))
    }
    if(endDate > max(custom.data$Date)){
      warning(paste('Data are available ending',
                    max(custom.data$Date), 'given user-defined parameters.'))
    }
  }

  return(custom.data)

}
