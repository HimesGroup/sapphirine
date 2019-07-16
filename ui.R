
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

source("global.R")

#Define user interface
ui <- fluidPage(
  
  titlePanel("Sensor-based Analysis of Polution in the Philadelphia Region with Information on Neighborhoods and the Environment (SAPPhiRINE)"),
  
  fluidRow(
    
    column(12,
           tags$p("SAPPhiRINE is an interactive geospatial-analysis tool that allows users to visualize pollution data throughout Philadelphia as recorded by portable sensors.
                  We include data downloaded from Aircasting's CrowdMap, data downloaded from the PurpleAir website, and data collected independently with our own sensors."),
           tags$p("Adjust the parameters below to your desired values, then click \"Go\" to display the corresponding map.
                  On the map below, click on a bin to view the data corresponding to the region it encompasses as well as to your selected parameters.
                  Click on the crosshair icon to recenter the map.")
           )
   ),
  
  wellPanel(
    
    fluidRow(
      
      column(2,
             #Dropdown list for selecting data by sensor
             pickerInput("sensors.hl",
                         label = "Himes Lab sensors to include",
                         choices = subset(sensor.names, sensor.names %in% our.sensors),
                         multiple = TRUE,
                         selected = subset(sensor.names, sensor.names %in% our.sensors),
                         options = list(`actions-Box` = TRUE, `none-Selected-Text` = "None")
             ),
             
             pickerInput("sensors.o",
                         label = "Other sensors to include",
                         choices = subset(sensor.names, !sensor.names %in% our.sensors),
                         multiple = TRUE,
                         selected = subset(sensor.names, !sensor.names %in% our.sensors),
                         options = list(`actions-Box` = TRUE, `none-Selected-Text` = "None")
             )
             
      ), #End column 1
      
      column(2,
             
             #Dual textbox for entering in a date range
             dateRangeInput("dates", label = "Date range", start = "2015-06-01", end = "2019-05-31", startview = "decade",
                            min = "2015-06-01", max = "2019-05-31"),
             
             #Slider bar for selecting a time-of-day-range
             sliderTextInput("times",
                             label = "Time of day",
                             choices = hours,
                             selected = c("00:00", "23:59"),
                             force_edges = TRUE,
                             width = "50%"
             )
             
      ), #End column 2
      
      column(2,
             
             #Slider for selecting raster boundaries
             sliderInput("lat.range",
                         label = "Latitude Range",
                         min = 38.85,
                         max = 40.60,
                         value = c(39.87, 40.14),
                         step = 0.00001,
                         sep = ""
                         ),
             
             sliderInput("lon.range",
                         label = "Longitude Range",
                         min = -76.40,
                         max = -74.40,
                         value = c(-75.28, -74.97),
                         step = 0.00001,
                         sep = ""
                         )
      ), #End column 3
      
      column(2,
             
             #Slider for selecting number of bins
             sliderInput("row.no",
                         label = "Rows",
                         min = 2,
                         max = 200,
                         value = 100
                        ),
             
             sliderInput("col.no",
                         label = "Columns",
                         min = 2,
                         max = 200,
                         value = 100
                         )
             
      ), #End column 4
      
      column(2,
             actionButton("go", "Go"),
             downloadButton("ss", "Export map image")
      ) #End column 5
      
    ) #FluidRow1
    
    #Checklist for selection of measurement type
    
  ), #End WellPanel
  
  fluidRow(
    leafletOutput("int.map", height = 700)
  )
  
           ) #End UI