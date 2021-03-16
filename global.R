
library(shiny)
library(lubridate)
library(dplyr)
library(shinyWidgets)
library(leaflet)
library(raster)
library(shinyjs)
library(mapview)
library(data.table)
library(feather)
library(RColorBrewer)
library(DescTools)
library(stringr)
library(tools)
library(gstat)
library(prevR)
library(leafsync)

app.data <- read_feather("databases/local_data.feather")

hours <- c("00:00",
           "01:00",
           "02:00",
           "03:00",
           "04:00",
           "05:00",
           "06:00",
           "07:00",
           "08:00",
           "09:00",
           "10:00",
           "11:00",
           "12:00",
           "13:00",
           "14:00",
           "15:00",
           "16:00",
           "17:00",
           "18:00",
           "19:00",
           "20:00",
           "21:00",
           "22:00",
           "23:00",
           "23:59"
            )

mins <- as_datetime(hm('0:00')): as_datetime(hm('23:59')) %>%
  as_datetime() %>%
  force_tz(tz = 'UTC') %>%
  strftime(format = '%H:%M') %>%
  unique()
#Creates character list that will be indexed for temporal subsetting
#Use UTC for online and New York for local

sensor.measures <- c("Temperature", "Humidity", "PM1", "PM2.5", "PM10")
other.measures <- c("Crime", "Area Deprivation Index", "Traffic")
all.measures <- c(sensor.measures, other.measures)

#Subscripted version of measurements
sensor.measures.sub <- c("Temperature", "Humidity", "PM\u2081", 
                        "PM\u2082.\u2085", "PM\u2081\u2080")
all.measures.sub <- c(sensor.measures.sub, other.measures)

titles.list <- c("Avg. Temp. (\u00B0C)", "Avg. Humidity (%)", 
                 "Avg. PM\u2081 Conc. (\u03BCg/m\u00B3)", 
                 "Avg. PM\u2082.\u2085 Conc. (\u03BCg/m\u00B3)", 
                 "Avg. PM\u2081\u2080 Conc. (\u03BCg/m\u00B3)", 
                     "# of Reported Crimes", "Avg. ADI", "Avg. AADT")

suffix.list <- c(".t", ".h", ".pm1", ".pm2.5", ".pm10", ".c", ".pov", ".tr")

titles.df <- data.frame(cbind(all.measures, titles.list, suffix.list))

epa.titles.df <- cbind(c("SO2", "NO2", "O3", "CO", "PM2.5", "PM10"),
                       c("Avg. SO\u2082 Conc. (ppb)", "Avg. NO\u2082 Conc. (ppb)", 
                         "Avg. O\u2083 Conc. (ppb)", "Avg CO Conc. (ppm)",
                         "Avg. PM\u2082.\u2085 Conc. (\u03BCg/m\u00B3)", 
                         "Avg. PM\u2081\u2080 Conc. (\u03BCg/m\u00B3)")
                       )

f.titles <- function(y){
  index <- which(titles.df[,1] == y)
  return(titles.df[index, 2])
}

f.plaintext <- function(b){
  if(b == 'PM\u2082.\u2085'){return('PM2.5')}
  else if(b == 'PM\u2081'){return('PM1')}
  else if(b == 'PM\u2081\u2080'){return('PM10')}
  else{return(b)}
}

f.titles.d <- function(w){paste("log\u2081\u2080 # of", w, "data points")}

f.titles.epa <- function(a){
  index <- which(epa.titles.df[,1] == a)
  return(epa.titles.df[index, 2])
}

f.suffix <- function(z){
  index <- which(titles.df[,1] == z)
  return(titles.df[index, 3])
}

f.zoom <- function(x, y){
  val <- ifelse(x > y, x, y)
  return(as.integer(round(11.47 - 1.5*val, digits = 0)))
}

f.top <- function(x){
  no.string <- toString(as.integer(x))
  lead.digit <- as.numeric(substr(no.string, 1, 1))
  no.digits <- nchar(no.string)
  if(lead.digit == 1){
    if(x == 100){
      return(100)
    }
    else{
      return(RoundTo(x, multiple = 2*10**(no.digits - 2), FUN = ceiling))
    }
  }
  else if(lead.digit >= 2 && lead.digit <= 4){
    return(RoundTo(x, multiple = 5*10**(no.digits - 2), FUN = ceiling))
  }
  else if(lead.digit >= 5){
    return(RoundTo(x, multiple = 10**(no.digits - 1), FUN = ceiling))
  }
}

pov.shp <- shapefile("databases/ADI_data.shp")

our.sensors <- fread("databases/LIMEA_AIRBEAM_SUMMARY.csv", 
                     header = TRUE, stringsAsFactors = FALSE)$AirBeamID[1:15]
our.sensors <- paste0("AirBeam:", our.sensors)

sensor.names <- levels(app.data$Sensor.ID)

county.borders <- shapefile("databases/gpa_counties/gpa_counties.shp") %>%
  spTransform(CRSobj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

city.border <- county.borders[county.borders$NAME == 'Philadelphia',]

traffic.raster <- raster("databases/traffic_raster.grd")

#Read in EPA data frames
EPA_data <- read_feather('./databases/EPA_data.feather')

#EPA raster function
getEPAraster <- function(variable, dates){
  
  col.name <- names(EPA_data)[grep(variable, names(EPA_data))]
  
  dat <- EPA_data %>%
    dplyr::select(1:9, .data[[col.name]]) %>%
    dplyr::filter(Date %in% dates) %>%
    dplyr::filter(!is.na(.data[[col.name]]), .data[[col.name]] >= 0)
  
  assign('epa.df', dat, envir = .GlobalEnv) 
  #Creates subsetted frame to be downloaded
  
  dat <- dat %>%
    dplyr::group_by(Latitude, Longitude) %>%
    dplyr::summarise(avg = mean(.data[[col.name]], na.rm = TRUE))
  
  ## Gives averages of daily-averaged values over range of dates (mean of mean-values)
  ## Get one row per location by averaging all entries
  
  if(nrow(dat) > 0){
    coordinates(dat) = ~Longitude+Latitude
    crs(dat) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    
    ## make base raster
    #find distance between the latitude and longitudes and convert to km (*111 for 1km)
    r <- raster(nrow = 451, ncol = 736, extent(-80.51985, -73.88506, 38.45113, 42.51607)) #1km
    crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    ## generate raster (idw with 5 nearest sites)
    gs <- gstat(formula=avg~1, data=dat, nmax = 5)
    nn <- interpolate(r, gs)
    
    return(nn)
  } else{
    print("zero")
    #find distance between the latitude and longitudes and convert to km (*111 for 1km)
    r <- raster(nrow = 451, ncol = 736, extent(-80.51985, -73.88506, 38.45113, 42.51607)) #1km
    crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    values(r) <- rep(-1, 331936)
    return(r)
  }
}

#Function for adding circles to EPA monitor legend
addLegendEPA <- function(map, colors, labels, sizes, opacity = 0.8){
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, 
                           "px;margin-top: 4.5px;height:", sizes, "px")  
  labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, 
                           "px;margin-top: 0px;line-height: ", sizes, "px;'>", 
                           labels, "</div>")
  
  return(addLegend(map, colors = colorAdditions, 
                   labels = labelAdditions, opacity = opacity))
}

#Reverse legend direction
myLabelFormat = function(prefix = "", suffix = "", between = " &ndash; ", digits = 3, 
                         big.mark = ",", transform = identity, t.val = Inf) {
  formatNum <- function(x) {
    format(round(transform(x), digits), trim = TRUE, scientific = FALSE, 
           big.mark = big.mark)
  }
  function(type, ...) {
    switch(type, numeric = (function(cuts) {
      cuts <- sort(cuts, decreasing = T) #just added
      paste0(prefix, formatNum(cuts), ifelse(cuts == t.val, "+", ""))
    })(...), bin = (function(cuts) {
      n <- length(cuts)
      paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]), 
             suffix)
    })(...), quantile = (function(cuts, p) {
      n <- length(cuts)
      p <- paste0(round(p * 100), "%")
      cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
      paste0("<span title=\"", cuts, "\">", prefix, p[-n], 
             between, p[-1], suffix, "</span>")
    })(...), factor = (function(cuts) {
      paste0(prefix, as.character(transform(cuts)), suffix)
    })(...))
  }
}