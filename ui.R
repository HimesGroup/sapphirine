
.libPaths("/home/maya/R/x86_64-pc-linux-gnu-library/3.4/") #mapview dependencies, use only for online version

#Define user interface
ui <- fluidPage(
  
  titlePanel("Sensor-based Analysis of Pollution in the Philadelphia Region with Information on Neighborhoods and the Environment (SAPPhiRINE)"),
  
  fluidRow(
    
    column(12,
           tags$p("SAPPhiRINE is an interactive geospatial-analysis tool that allows users to visualize pollution and other data throughout the Greater Philadelphia Area with high degrees of geographic and temporal specificity."),
           tags$p("Adjust the parameters below to your desired values, and then click \"Go\" to display the corresponding map. Slider bars can be fine-tuned using arrow keys.
                  Within the map, you will be able to switch between interactive displays for the measurements listed in the upper right corner, and, for sensor measures, you will be able to visualize either the measurement value or the measurement density.
                  Click on a bin to view the data corresponding to the region the bin encompasses, in accordance with your set parameters. For each data type, the measurement value is given, and where applicable, the number of data points used to estimate the average value (i.e. the measurement density) is given in parentheses.
                  Click on the ruler icon to measure physical distances on the map, and click on the crosshair icon to recenter the map.
                  The latitude and longitude corresponding to the cursor position are shown above the top left corner of the map, which is useful for selecting the desired geographic scope of the display.
                  To take a screenshot of the map, use Windows+PrtSc (Windows) or Shift+Command+5 (Mac)."),
           tags$p("The map is optimized to allow for immediate switching between different data displays, which requires longer loading times, particularly for higher resolution displays.
                  Please allow up to a few minutes for the map to load."),
           tags$p("For information on our data sources and code, visit ",
                  tags$a(href = "https://github.com/HimesGroup/sapphirine", "https://github.com/HimesGroup/sapphirine", target = "_blank")),
           tags$p("Note: Traffic and poverty data are not subject to temporal subsetting.")
           )
   ),
  
  wellPanel(
    
    fluidRow(
      
      column(3,
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
             ),
             
             actionButton("go", "Go")
             
      ), #End column 1
      
      column(3,
             
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
      
      column(3,
             
             #Slider for selecting raster boundaries
             sliderInput("lat.range",
                         label = "Latitude Range",
                         min = 38.85,
                         max = 40.60,
                         value = c(39.87, 40.17),
                         step = 0.00001,
                         sep = ""
                         ),
             
             sliderInput("lon.range",
                         label = "Longitude Range",
                         min = -76.40,
                         max = -74.40,
                         value = c(-75.28, -74.96),
                         step = 0.00001,
                         sep = ""
                         )
      ), #End column 3
      
      column(3,
             
             #Slider for selecting number of bins
             sliderInput("row.no",
                         label = "Rows",
                         min = 2,
                         max = 200,
                         value = 60
                        ),
             
             sliderInput("col.no",
                         label = "Columns",
                         min = 2,
                         max = 200,
                         value = 64
                         )
             
      ) #End column 4
      
    ) #FluidRow1
    
  ), #End WellPanel
  
  fluidRow(
    
    column(1),
    
    column(10,
           
      leafletOutput("int.map", height = 700)
           
    ),
    
    column(1)
    
  )
  
           ) #End UI