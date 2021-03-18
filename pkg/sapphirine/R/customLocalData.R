#' Function
#' @export
#' @examples
#' customLocalData()

customLocalData <- function(shape = sapphirine::GPA_counties, startDate, endDate,
                            startTime = '00:00', endTime = '23:59',
                            sensors = sapphirine::sensor.list){

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

  mins <- as_datetime(hm('0:00')):as_datetime(hm('23:59')) %>%
    as_datetime() %>%
    force_tz(tz = 'UTC') %>%
    strftime(format = '%H:%M') %>%
    unique()

  custom.data <- sapphirine::local.data %>%
    dplyr::filter(
      point.in.SpatialPolygons(Longitude, Latitude, shape),
      lubridate::date(Timestamp) %in% startDate:endDate,
      strftime(Timestamp, format = '%H:%M', tz = 'America/New_York') %in%
        mins[grep(startTime, mins) : grep(endTime, mins)])

  custom.data[which(!is.na(custom.data$Sensor.ID)),] <-
    dplyr::filter(custom.data[which(!is.na(custom.data$Sensor.ID)),],
                  Sensor.ID %in% sensors)

  if(startDate < lubridate::date(min(custom.data$Timestamp))){
    warning(paste('Data are available beginning',
                   lubridate::date(min(custom.data$Timestamp)), '
                  given user-defined parameters.'))
  }
  if(endDate > lubridate::date(max(custom.data$Timestamp))){
    warning(paste('Data are available ending',
                  lubridate::date(max(custom.data$Timestamp)), '
                  given user-defined parameters.'))
  }

  return(custom.data)

}
