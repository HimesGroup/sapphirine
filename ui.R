
#Define user interface
ui <- fluidPage(

  useShinyjs(),
  
  tags$head(
    tags$style('body {font-family: "Lucida Sans Unicode", "Lucida Grande", sans-serif;}')
  ),
  
  tags$head(
    tags$style('h1 {font-family: "Lucida Console", Monaco, monospace; text-align: center;
                    color: #337AB7;}')
  ),
  
  tags$head(
    tags$style('h3 {font-family: "Lucida Console", Monaco, monospace; text-align: center;}')
  ),
  
  tags$head(
    tags$style('hr {border-width:1.5px;}')
  ),
  
  tags$head(
    tags$style('p {font-family: "Lucida Sans Unicode", "Lucida Grande", sans-serif;}')
  ),
  
  #CSS class for tabs
  tags$style(HTML("
    .tabbable > .nav > li > a {background-color: #f5f5f5;}
    .tabbable > .nav > li[class=active] > a {background-color: #f5f5f5; color:#333}
  ")),
  
  #Use h6 to align Go buttons properly
  tags$head(
    tags$style('h6 {font-size: 4.7px;}')
  ),
  
  br(),
  
  wellPanel(
  
    h1(strong("SAPPHIRINE")),
    h3("Sensor-based Analysis of Pollution in the Philadelphia Region", br(), 
       "with Information on Neighborhoods and the Environment")
    ),
  

  
  fluidRow(
    
    column(12,
           tags$p("SAPPHIRINE is an interactive geospatial-analysis tool that allows users to visualize pollution and other data throughout the Greater Philadelphia Area with high degrees of geographic and temporal specificity."),
           tags$p("For information on our data sources and code, visit ",
                  tags$a(href = "https://github.com/HimesGroup/sapphirine", "https://github.com/HimesGroup/sapphirine", target = "_blank"))           )
    ),
  
  br(),       
  
  fluidRow(
    HTML('&#8287&#8287&#8287&#8287'), #Adds spaces
    actionButton('Instructions_b1', 'Show/Hide Instructions')
  ),
  
  HTML('&#8287'),
  
  hidden(
    
    fluidRow(id = 'Instructions_1',
             
             column(12,
                    
                    tags$p("Adjust the parameters below to your desired values, and then click \"Go\" to display the corresponding maps. Slider bars can be fine-tuned using arrow keys."),
                    tags$p("Within \"Main Map,\" you can switch between interactive displays for the measurements listed in the upper right corner, and, for sensor measures, you can visualize either the measurement value or the measurement density.
                    Click on a bin to view the data corresponding to the region the bin encompasses, in accordance with your set parameters. For each data type, the measurement value is given, and where applicable, the number of data points used to estimate 
                    the average value (i.e. the measurement density) is given in parentheses."),
                    tags$p("Click on the ruler icon to measure physical distances on the map, and click on the crosshair icon to recenter the map.
                    The latitude and longitude corresponding to the cursor position are shown above the top left corner of the map, which is useful for selecting the desired geographic scope of the display.
                    To take a screenshot of the map, use Windows+PrtSc (Windows) or Shift+Command+5 (Mac)."),
                    tags$p("Click on \"Download original data\" to download all sensor and crime data points used to create the map display. Click \"Download raster data\" to download the averaged points for all variables as displayed on the map.
                 Be sure to click \"Go\" before attempting to download data, as the data to be downloaded correspond to those rendered on the map."),
                    tags$p("Under \"EPA Map,\" Select a pollutant for which to view an interpolated map display based on measurement data from the U.S. Environmental 
                   Protection Agency (EPA) over the specified date range. EPA monitor locations are indicated by blue dots, and the map estimates
                   are calculated using inverse-distance-weighted interpolation. Click on any blue dot for information about the corresponding monitoring site, and click on any location within the map display to retrieve the interpolated pollution value. 
                           Click \"Download EPA data\" to download the EPA monitor data used to render the display.  Click \"Go\" to register a change in the date range."),
                    tags$p("Under \"Grid Map,\" choose from the list \"Plot variables\" and click\"Update grid\"  to display side-by-side synchronized map displays corresponding to the specified parameters. 
                           Click \"Go\" to register any changes in parameters."),
                    tags$p("Note: Traffic and poverty data are not subject to temporal subsetting.")
                    
             )
    )
  ),
                    
      wellPanel(
        
        fluidRow(
          
          column(3,
                 #Dropdown list for selecting data by sensor
                 pickerInput("sensors",
                             label = "Low-cost pollution sensors to include",
                             choices = sensor.names,
                             multiple = TRUE,
                             selected = sensor.names,
                             options = list(`actions-Box` = TRUE, `none-Selected-Text` = "None",
                                            `selected-Text-Format`= "count")
                 ),
                 
                 br(),
                 
                 fluidRow(
                   column(2, actionButton("go", "Go")),
                   column(4, downloadButton('download', 'Download original data'))
                 ),
                 
                 br(),
                 br(),
                 
                 fluidRow(
                   column(6,
                          downloadButton('download.ras', 'Download raster data'))
                 )
                 
          ), #End column 1
          
          column(3,
                 
                 #Dual textbox for entering in a date range
                 dateRangeInput("dates", label = "Date range", start = "2015-06-01", end = "2019-12-31", startview = "decade",
                                min = "2015-06-01", max = "2019-12-31"),
                 
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
        
      ),#End WellPanel
      
      tabsetPanel(tabPanel("Main Map", br(),  
                           
                           column(1),
                           
                           column(10, leafletOutput("int.map", height = 700)),
                           
                           column(1)),
                  
                  tabPanel("EPA Map",
                           
                           fluidRow(
                             
                             column(8, align = "center", offset = 3,
                                    h6(HTML('&#8287')),
                                    column(4,
                                      pickerInput('EPA_var',label = "Pollutant",
                                                  choices = list("PM\u2082.\u2085" = "PM2.5", 
                                                                 "PM\u2081\u2080" = "PM10", 
                                                                 "SO\u2082" = "SO2", 
                                                                 "NO\u2082" = "NO2", 
                                                                 "O\u2083" = "O3", 
                                                                 "CO"))
                                    ),
                                    
                                    column(2,
                                           h6(HTML('&#8287')),
                                           actionButton("EPA_go", "Go")
                                    ),
                                    column(2,
                                           h6(HTML('&#8287')),
                                           downloadButton('EPA_download', 'Download EPA data')
                                    )

                             )
                             
                           ),
                           
                           column(1),
                           
                           column(10, leafletOutput("int.map.epa", height = 700)),
                           
                           column(1)
                           
                  ), #End EPA tab panel
                  
                  tabPanel("Grid Maps", 
                           
                           fluidRow(
                             column(6, align="center", offset = 4,
                                    h6(HTML('&#8287')),
                                    column(4,
                                    pickerInput('grid.vars',
                                                label = 'Plot variables',
                                                choices = list(
                                                  "PM\u2082.\u2085" = "pm25", 
                                                  "PM\u2081" = "pm1",
                                                  "PM\u2081\u2080" = "pm10",
                                                  "Temperature" = "temp",
                                                  "Humidity" = "humid",
                                                  "Crime" = "crime",
                                                  "Area Deprivation Index" = 'pov',
                                                  "Traffic" = 'tr',
                                                  "PM\u2082.\u2085 density plot" = "pm25.d", 
                                                  "PM\u2081 density plot" = "pm1.d",
                                                  "PM\u2081\u2080 density plot" = "pm10.d",
                                                  "Temperature density plot" = "temp.d",
                                                  "Humidity density plot" = "humid.d",
                                                  "Int. EPA PM\u2082.\u2085 plot" = 'epa.pm25',
                                                  "Int. EPA PM\u2081\u2080 plot" = 'epa.pm10',
                                                  "Int. EPA SO\u2080\u2082 plot" = 'epa.so2',
                                                  "Int. EPA NO\u2080\u2082 plot" = 'epa.no2',
                                                  "Int. EPA O\u2083 plot" = 'epa.o3',
                                                  "Int. EPA CO plot" = 'epa.co'
                                                ),
                                                multiple = TRUE,
                                                selected = c('pm25', 'temp', 'humid'),
                                                options = list(`actions-Box` = TRUE, `none-Selected-Text` = "None",
                                                               `selected-Text-Format`= "count"))),
                                    column(2,
                                           h6(HTML('&#8287')),
                                           actionButton('grid.update', 'Update Grid')
                                    )
                                    )),
                           
                           br(),  
                           column(1),
                           
                           column(10, uiOutput("all.maps", height = 700)),
                           
                           column(1),
                           br(), br())
                  
      ) #End inner tabPanel and tabsetPanel

) #End UI