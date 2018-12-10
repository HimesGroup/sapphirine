#Define user interface
ui <- fluidPage(
  
  titlePanel("Sensor-based Analysis of Polution in the Philadelphia Region (SAPPhiRe)"),
  
  fluidRow(
    
    column(12,
           tags$p("SAPPhiRe is an interactive geospatial-analysis tool that allows users to visualize pollution data throughout Philadelphia as recorded by portable sensors.
                  We include data downloaded from Aircasting's CrowdMap, data downloaded from the PurpleAir website, and data collected independently with our own sensors."),
           tags$p("Adjust the parameters below to your desired values, then click \"Go\" to display the corresponding map.
                  On the map below, click on a bin to view the data corresponding to the region it encompasses as well as to your selected parameters.
                  Click on the crosshair icon to recenter the map.")
           )
           ),
  
  wellPanel(
    
    fluidRow(
      
      column(3,
             pickerInput("vars",
                         label = "Plot",
                         choices = list(
                           "Temperature" = "Temperature",
                           "Humidity" = "Humidity",
                           "PM1" = "PM1",
                           "PM2.5 - continuous" = "PM2.5",
                           "PM2.5 - discrete" = "PM2.5.d",
                           "PM10" = "PM10"
                         ),
                         multiple = FALSE,
                         selected = "PM2.5"
             ),
             
             radioButtons("d.type",
                          label = "Display type",
                          choices = list(
                            "Measurement value" = "mv",
                            "Measurement density" = "md"
                          )
             )
             
      ), #End column 1
      
      column(3,
             #Dropdown list for selecting data by sensor
             pickerInput("sensors.hl",
                         label = "Himes Lab sensors to include",
                         choices = subset(sensor.names, sensor.names %in% our.sensors),
                         multiple = TRUE,
                         selected = sensor.names,
                         options = list(`actions-Box` = TRUE, `none-Selected-Text` = "None")
             ),
             
             pickerInput("sensors.o",
                         label = "Other sensors to include",
                         choices = subset(sensor.names, !sensor.names %in% our.sensors),
                         multiple = TRUE,
                         selected = sensor.names,
                         options = list(`actions-Box` = TRUE, `none-Selected-Text` = "None")
             )
             
      ),
      
      column(3,
             
             #Dual textbox for entering in a date range
             dateRangeInput("dates", label = "Date range", start = "2014-01-01", end = Sys.Date(), startview = "decade"),
             
             #Slider bar for selecting a time-of-day-range
             sliderTextInput("times",
                             label = "Time of day",
                             choices = mins.5,
                             selected = c("00:00", "23:59"),
                             force_edges = TRUE,
                             width = "50%"
             )
             
      ), #End column 2
      
      column(3,
             
             #Slider for selecting number of bins
             sliderInput("bin.no",
                         label = "Resolution (# of bins per row/column)",
                         min = 5,
                         max = 125,
                         value = 75
             ),
             
             fluidRow(
               column(3,
                      actionButton("go", "Go")
               ),
               column(6,
                      actionButton("ss", "Export map image")
               )
             )
             
      ) #End column 3
    ) #End WellPanel
    
    #Checklist for selection of measurement type
    
  ), #End row 1
  
  fluidRow(
    leafletOutput("int.map", height = 700)
  )
  
           ) #End UI