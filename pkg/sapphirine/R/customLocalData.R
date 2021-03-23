#' Custom Local Data
#'
#' Subset the sapphirine local data frame according to custom parameters
#'
#' @param shape A SpatialPolygons or extent object, for example, a subset of `sapphirine::GPA_counties` created using `sapphirine::selectGPACounties`, to define the geographic scope of data to include
#' @param startDate A `Date` object or `character` string of format yyyy-mm-dd defining the lower bound of the time frame over which to include data
#' @param endDate " " for the upper bound
#' @param startTime `character` string of format HH:MM defining the lower bound of the time of day in U.S. Eastern (New York) time over which to include data
#' @param endTime " " for the upper bound
#' @param sensors A`character` vector listing the names of all sensors for which to include data. See `sapphirine::sensor.list` for a list of sensor names
#' @param variables A `character` vector of geospatial variables to include
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
#' #Retrieve local data in selected counties from 1 June 2017 - 31 May 2019 over 16:00-18:00 U.S. Eastern (New York) time
#' customLocalData(cty.shape, '2017-06-01', '2019-05-31', '16:00', '18:00')

customLocalData <- function(shape = sapphirine::GPA_counties, startDate, endDate,
                            startTime = '00:00', endTime = '23:59',
                            sensors = sapphirine::sensor.list,
                            variables = c('Temperature', 'Humidity',
                                          'PM1', 'PM2.5','PM10',
                                          'Crime')){

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
        mins[grep(startTime, mins) : grep(endTime, mins)]) %>%
    dplyr::select('Timestamp', 'Latitude', 'Longitude', 'Sensor.ID', variables,
                  'Count')

  custom.data[which(!is.na(custom.data$Sensor.ID)),] <-
    dplyr::filter(custom.data[which(!is.na(custom.data$Sensor.ID)),],
                  Sensor.ID %in% sensors)

  custom.data <- custom.data[apply(custom.data[,variables], 1, function(x) sum(is.na(x))) != length(variables),]

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
