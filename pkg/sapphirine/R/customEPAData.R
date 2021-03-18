#' Function
#' @export
#' @examples
#' customEPAData()

customEPAData <- function(startDate, endDate, shape = sapphirine::GPA_counties,
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

  if(startDate < min(custom.data$Date)){
    warning(paste('Data are available beginning',
                  min(custom.data$Date), 'given user-defined parameters.'))
  }
  if(endDate > max(custom.data$Date)){
    warning(paste('Data are available ending',
                  max(custom.data$Date), 'given user-defined parameters.'))
  }

  return(custom.data)

}
