#' Interpolated Raster from EPA Data
#'
#' Create a brick of 1km x 1km raster layers rendered with inverse-distance-weighted interpolation of EPA data.
#'
#' @param data A subset of `sapphirine::EPA.data`. See `sapphirine::customLocalData` for easy custom subsetting.
#' @param shape A SpatialPolygons or extent object, for example, a subset of `sapphirine::GPA_counties` created using `sapphirine::selectGPACounties`, to define the boundaries of the raster layers.
#' @param power Power of inverse-distance-weighted interpolation (e.g., `power = 2` will call inverse-square-distance-weighted interpolation)
#' @param variables A vector of `sapphirine::EPA.data` variables to include.
#' @import raster
#' @import magrittr
#' @import gstat
#' @import dplyr
#' @import sp
#' @import lubridate
#' @export
#' @examples
#'
#' #Select counties for shape
#' counties <- c('Bucks', 'Chester', 'Delaware', 'Montgomery', 'Philadelphia')
#' cty.shape <- selectGPACounties(counties)
#'
#' #Customize subset of EPA.data
#' dat <- customEPAData(cty.shape, '2017-06-01', '2019-05-31')
#'
#' intEPARaster(dat, cty.shape)

intEPARaster <- function(data, shape, power = 1,
                         variables = c('PM2.5', 'PM10', 'SO2',
                                       'NO2', 'O3', 'CO')){

  if(class(shape) == 'Extent'){
    shape <- as(shape, 'SpatialPolygons')
    crs(shape) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  } else if(grepl('SpatialPolygons', class(shape))){
    crs(shape) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  } else if(!grepl('SpatialPolygons', class(shape))){
    stop('shape must be of class \"Extent,\" \"SpatialPolygons,\" or \"SpatialPolygonsDataFrame\"')
  }

  for(i in 1:length(variables)){

    col.name <- names(data)[grep(variables[i], names(data))]

    dat <- data %>%
      dplyr::select(1:9, .data[[col.name]]) %>%
      dplyr::filter(!is.na(.data[[col.name]]), .data[[col.name]] >= 0) %>%
      dplyr::group_by(Latitude, Longitude) %>%
      dplyr::summarise(avg = mean(.data[[col.name]], na.rm = TRUE))

    if(nrow(dat) > 0){
      coordinates(dat) = ~Longitude+Latitude
      crs(dat) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

      ## make base raster
      #find distance between the latitude and longitudes and convert to km (*111 for 1km)
      r <- raster(nrow = 451, ncol = 736, extent(-80.51985, -73.88506, 38.45113, 42.51607)) #1km
      crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      ## generate raster (idw with 5 nearest sites)
      gs <- gstat(formula=avg~1, data=dat, nmax = 5, set = list(idp = power))
      nn <- interpolate(r, gs)

      if(i == 1){
        ras.brick <- brick(nn)
      } else {
        ras.brick[[i]] <- nn
      }

    } else{
      print("zero")
      #find distance between the latitude and longitudes and convert to km (*111 for 1km)
      r <- raster(nrow = 451, ncol = 736, extent(-80.51985, -73.88506, 38.45113, 42.51607)) #1km
      crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      values(r) <- rep(-1, 331936)

      if(i == 1){
        ras.brick <- brick(r)
      } else {
        ras.brick[[i]] <- r
      }
    }

    names(ras.brick)[i] <- variables[i]

  } #End loop

  ras.brick[ras.brick == -1] <- NA #Non-values are given as -1 by raster function
  ras.brick <- crop(ras.brick, extent(shape)) %>% mask(shape)

  setClass('EPARasterBrick', contains = 'RasterBrick',
           slots = c(monitor_data = 'list'))

  ras.brick <- as(ras.brick, 'EPARasterBrick')

  for(j in 1:nlayers(ras.brick)){

    col.name <- names(data)[grep(variables[j], names(data))]

    mon.frame <- data %>%
      dplyr::select(1:9, .data[[col.name]]) %>%
      dplyr::filter(!is.na(.data[[col.name]]), .data[[col.name]] >= 0)

    mon.frame$Monitor_Start_Date[which(
      mon.frame$Monitor_Start_Date < min(mon.frame$Date))] <- min(mon.frame$Date)
    mon.frame$Last_Sample_Date[which(
      mon.frame$Last_Sample_Date > max(mon.frame$Date))] <- max(mon.frame$Date)

    mon.frame <- mon.frame %>%
      dplyr::select(Longitude, Latitude, AQS_Site_ID, Local.Site.Name,
                    State.Name, City.Name, Monitor_Start_Date, Last_Sample_Date) %>%
      unique()

    ras.brick@monitor_data[[j]] <- mon.frame

  }

  return(ras.brick)

}
