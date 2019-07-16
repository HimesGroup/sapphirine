.libPaths("/home/maya/R/x86_64-pc-linux-gnu-library/3.4/") #mapview dependencies, use only for online version
library(shiny)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggmap)
library(grDevices)
library(shinyWidgets)
library(grid)
library(gridExtra)
library(gtable)
library(leaflet)
library(raster)
library(shinyjs)
library(mapview)
library(data.table)
library(feather)

#Memory limit increase (online only)
#library(httr)
#library(ulimit)
#library(feather)
#set_config( config( ssl_verifypeer = 0L ))
#ulimit::memory_limit(4000)

#Use this one if online:
#app.data <- read_feather("/srv/shiny-server/databases/sapphirine/all_data.feather")

#Use this one if locally making adjustments:
#Local
#app.data <- read_feather("./databases/all_data.feather")

#Online
app.data <- read_feather("/srv/shiny-server/databases/cdatabases/sapphirine/all_data.feather")

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

mins <- unique(strftime(force_tz(as_datetime(as_datetime(hm("0:00")): as_datetime(hm("23:59"))), tz = "America/New_York"), format = "%H:%M"))
#Creates character list that will be indexed for temporal subsetting

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
 
} #End f.titles

f.titles.d <- function(w){paste("log # of", w, "data points")}

f.suffix <- function(z){
  
  index <- which(titles.df[,1] == z)
  return(titles.df[index, 3])
  
}

#Online:
pov.raster <- raster("/srv/shiny-server/databases/cdatabases/sapphirine/poverty.grd")

#Local:
#pov.raster <- raster('./databases/poverty.grd')

#Online:
our.sensors <- fread("/srv/shiny-server/databases/cdatabases/sapphirine/LIMEA_AIRBEAM_SUMMARY.csv", header = TRUE, stringsAsFactors = FALSE)$AirBeamID[1:15]

#Local:
#our.sensors <- fread("./databases/LIMEA_AIRBEAM_SUMMARY.csv", header = TRUE, stringsAsFactors = FALSE)$AirBeamID[1:15]

#Online:
city.border <- read.csv("/srv/shiny-server/databases/cdatabases/sapphirine/city_border.csv", header = TRUE)[,2:3]

#Local:
#city.border <- read.csv("./databases/city_border.csv", header = TRUE)[,2:3]

#Online:
traffic.df <- readRDS("/srv/shiny-server/databases/cdatabases/sapphirine/phlroads.rds")

#Local:
#traffic.df <- readRDS("./databases/phlroads.rds")

traffic.df$CUR_AADT <- as.numeric(as.character(traffic.df$CUR_AADT))
traffic.df <- spTransform(traffic.df, CRS("+proj=longlat +datum=WGS84"))

our.sensors <- paste0("AirBeam:", our.sensors)

sensor.names <- names(table(app.data$Sensor.ID))
