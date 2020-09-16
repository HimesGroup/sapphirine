
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

app.data <- read_feather("databases/all_data.feather")

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

mins <- unique(strftime(force_tz(as_datetime(as_datetime(hm("0:00")): as_datetime(hm("23:59"))), tz = "UTC"), format = "%H:%M"))
#Creates character list that will be indexed for temporal subsetting
#Use UTC for online and New York for local

sensor.measures <- c("Temperature", "Humidity", "PM1", "PM2.5", "PM10")
other.measures <- c("Crime", "Poverty", "Traffic")
all.measures <- c(sensor.measures, other.measures)

titles.list <- c("Avg. Temp. (\u00B0C)", "Avg. Humidity (%)", "Avg. PM1 Conc. (\u03BCg/m\u00B3)", 
                     "Avg. PM2.5 Conc. (\u03BCg/m\u00B3)", "Avg. PM10 Conc. (\u03BCg/m\u00B3)", 
                     "# of Reported Crimes", "Avg. ADI", "Avg. AADT")

suffix.list <- c(".t", ".h", ".pm1", ".pm2.5", ".pm10", ".c", ".pov", ".tr")

titles.df <- data.frame(cbind(all.measures, titles.list, suffix.list))

f.titles <- function(y){
  index <- which(titles.df[,1] == y)
  return(titles.df[index, 2])
}

f.titles.d <- function(w){paste("log # of", w, "data points")}

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

pov.raster <- raster("databases/poverty.grd")

our.sensors <- fread("databases/LIMEA_AIRBEAM_SUMMARY.csv", header = TRUE, stringsAsFactors = FALSE)$AirBeamID[1:15]

city.border <- read.csv("databases/city_border.csv", header = TRUE)[,2:3]

traffic.raster <- raster("databases/traffic_raster.grd")

our.sensors <- paste0("AirBeam:", our.sensors)

sensor.names <- levels(app.data$Sensor.ID)
