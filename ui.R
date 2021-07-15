
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
    HTML('&#8287&#8287&#8287&#8287'), #Adds spaces
    actionButton('Instructions_b1', 'Show/Hide Instructions')
  ),
  
  HTML('&#8287'),
  
  hidden(
    
    fluidRow(id = 'Instructions_1',
             
             column(10, offset = 1,
                    
                    fluidRow(
                    tags$p("SAPPHIRINE is an interactive geospatial-analysis tool that allows users to visualize pollution and other data throughout the Greater Philadelphia Area with high degrees of geographic and temporal specificity."),
                    tags$p("For information on our data sources and code, visit ",
                    tags$a(href = "https://github.com/HimesGroup/sapphirine", "https://github.com/HimesGroup/sapphirine", target = "_blank")),
                    tags$p(HTML(paste0("Adjust the parameters below to your desired values, and then click \"Go\" to display the corresponding maps. Main Map data types include PM", tags$sub('2.5'), ", PM", tags$sub('1'), ", and PM", tags$sub('10'), " concentrations, sensor temperature and relative humidity, reported crimes (Philadelphia only), Area Deprivation Index (ADI), Average Annual Daily Traffic (AADT, Pennsylvania only), and % Tree Cover. ",
                                       "EPA data types include PM", tags$sub('2.5'), ", PM", tags$sub('10'), ", SO", tags$sub('2'), ", NO", tags$sub('2'), ", O", tags$sub('3'), ", and CO concentrations."))),
                    tags$p(HTML(paste0(strong('Geographic Boundary:')))),
                    tags$p('Type: Select whether you would like to subset data to a set of counties, a set of ZIP codes, or a rectangular geographic region.'),
                    tags$p('Scope: Select from a list of counties, enter a list of ZIP codes separated by a comma and space, or define a region of latitude/longitude. For a rectangular boundary, the slider bars can be fine-tuned using the arrow keys.'),
                    tags$p(HTML(paste0(strong('Time Frame:')))),
                    tags$p(HTML(paste0('Note that ', strong('Traffic, Area Deprivation Index, and Tree Cover data are not subject to temporal subsetting.')))),
                    tags$p('Date Range: Define a range of dates within 1 June 2015 - 31 December 2019 over which to include data.'),
                    tags$p(HTML(paste0('Time of Day: If desired, restrict the data to a certain time range during each day of the date range. ', strong("Does not apply to EPA data.")))),
                    tags$p(HTML(paste0(strong('Data Display Settings:')))),
                    tags$p(HTML(paste0('Low-cost pollution sensors: Select individual low-cost pollution sensors by ID for which to include data. AirBeam and AirBeam2 are mobile personal sensors manufactured by HabitatMap which measure PM', tags$sub('2.5'), ' concentrations, temperature, and relative humidity. ',
                                       'AirBeam2 sensors additionally measure PM', tags$sub('1'), ' and PM', tags$sub('10'), ' concentrations. ',
                                       'Data are from the manufacturer ', tags$a(href = "http://aircasting.habitatmap.org/mobile_map", "crowdsourced database", target = "_blank"), '. ',
                                       'PurpleAir sensors take the same measurements as AirBeam2 sensors but are stationary. Data originate from the PurpleAir, ', tags$a(href = 'https://www.purpleair.com/sensorlist', 'crowdsourced database'), '.'))),
                    tags$p(HTML(paste0('Geospatial Bin Type: Choose which type of bin over which to summarize data in the Main Map. If rectangular, slider bars will appear and allow for custom resolution. Rectangular bins are particularly advantageous for geospatial granularity. Within each bin, crime reports will be summed and all other data types averaged. If the geographical boundary cuts off part of a bin, data will be included only within the area of the bin ', em('inside'), ' of the boundary.'))),
                    tags$p(HTML(paste0('Download Displayed Data: After clicking \"Go\", you\'ll have the options to download 1\\) the raw sensor and crime data used to render the Main Map, 2\\) the binned data as summarized and displayed on the Main Map, and 3\\) the EPA data used to render the EPA Map.'))),
                    tags$p(HTML(paste0(strong("Main Map:"), " Switch among interactive displays of the Main Map variables described above using the list in the upper right corner of the map. For sensor measures, choose between the measurement value or the measurement density.
                    Click on a bin to view a list of summary data. Where applicable, the measurement density, i.e. the number of data points used to estimate 
                    a summary average, is given in parentheses."))),
                    tags$p(HTML(paste0(strong('EPA Map:'), ' Switch among interactive displays of the EPA Map variables described above using the list in the upper right corner of the EPA Map. Data are recorded at the locations indicated by the blue dots \\(which can be toggled by checking/unchecking \"Monitor Locations\"\\) and are rendered using inverse-distance-weighted interpolation from the five stations nearest any point. ', 
                                       'Note that monitors outside of the geographical boundary still appear on the map and map be used to interpolate values within the boundary. Click on any blue dot for information on the corresponding EPA monitoring site, and click on any location in the display for an interpolated estimate of the selected variable.'))),
                    tags$p(HTML(paste0(strong('Grid Map:'), 'Select any number of layers to be displayed alongside one another from all map layers of the Main Map and EPA Map.'))),
                    tags$p("On the Main Map and EPA Map, check/uncheck \"Counties,\" \"ZIP Codes,\" or \"Block Groups\" to display the corresponding boundary lines. Click on the ruler icon to measure physical distances. On any map, click on the crosshair icon to recenter the view.
                    The latitude and longitude corresponding to the cursor position are shown above the top left corner of the Main and EPA Maps.
                    To take a screenshot of the map, use Windows+PrtSc (Windows) or Shift+Command+5 (Mac).")
             )
             )
    )
  ),
                    
      wellPanel(
        
        fluidRow(
          
          column(3,
                 
                 h4(strong('Geographic Boundary'), align = 'center'),
                 
                 pickerInput('geoBounds',
                             'Type',
                             choices = list(
                               'Rectangle',
                               'Counties',
                               'ZIP Codes'
                             ),
                             multiple = FALSE,
                             selected = 'Counties'),
                 
                 h5(strong('Scope:')),
                 
                 conditionalPanel(condition = 'input.geoBounds == \"Rectangle\"',
                                  
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
                                  
                                  
                                  
                 ), #End "Rectangle" panel
                 
                 conditionalPanel(condition = 'input.geoBounds == \"Counties\"',
                                  
                                  pickerInput('cty.bounds',
                                              label = 'Select Counties',
                                              choices = GPACountyNames,
                                              multiple = TRUE,
                                              selected = 'Philadelphia',
                                              options = list(`actions-Box` = TRUE, `none-Selected-Text` = "None")
                                  )
                                  
                 ),
                 
                 conditionalPanel(condition = 'input.geoBounds == \"ZIP Codes\"',
                                  
                                  textInput('zip.bounds',
                                            label = 'Enter ZIPs (separate by \", \")'),
                                  value = ''
                                  
                 )
                 
          ), #End column
          
          column(3,
                 
                 h4(strong('Time Frame'), align = 'center'),
                 
                 #Dual textbox for entering in a date range
                 dateRangeInput("dates", label = "Date range", start = "2015-06-01", end = "2019-12-31", startview = "decade",
                                min = "2015-06-01", max = "2019-12-31"),
                 
                 #Slider bar for selecting a time-of-day-range
                 sliderTextInput("times",
                                 label = "Times of day",
                                 choices = hours,
                                 selected = c("00:00", "23:59"),
                                 force_edges = TRUE,
                                 width = "50%"
                 )
                 
          ), #End column
          
          column(3,
                 
                 h4(strong('Data Display Settings'), align = 'center'),
                 
                 #Dropdown list for selecting data by sensor
                 pickerInput("sensors",
                             label = "Low-cost pollution sensors",
                             choices = sensor.names,
                             multiple = TRUE,
                             selected = sensor.names,
                             options = list(`actions-Box` = TRUE, `none-Selected-Text` = "None",
                                            `selected-Text-Format`= "count")
                 ),
                 
                 br(),
                 
                 pickerInput('binType',
                             'Geospatial Bin Type',
                             choices = list(
                               'Rectangles',
                               'ZIP Codes',
                               'Block Groups',
                               'Counties'
                             ),
                             multiple = FALSE,
                             selected = 'ZIP Codes'),
                 
                 
                 conditionalPanel(condition = 'input.binType == \"Rectangles\"',
                                  
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
                                  
                                  
                 )
                 
          ), #End column
          
          column(3,
             
                 h4(strong('Download Displayed Data'), align = 'center'),
                 
                 br(),
                 
                 fluidRow(
                   column(4, downloadButton('download', 'Download raw sensor & crime data'),
                          align = 'center')
                 ),
                 
                 br(),

                 fluidRow(
                   column(6,
                          downloadButton('download.ras', 'Download binned raster data'),
                          align = 'center')
                 ),
                 
                 br(),
                 
                 fluidRow(
                   column(4,
                          downloadButton('EPA_download', 'Download EPA data'),
                          align = 'center')
                 )
                 

          ) #End column
          
        ) #FluidRow1
        
      ),#End WellPanel
 
 fluidRow(
   column(12,
          tags$p(HTML(paste0('Once you have adjusted above widgets to your desired parameters, click Go. Data downloads and map displays rely on the parameters specified ', em('before'), ' the most recent time Go was clicked. The intial map display and data downloads correspond to the initial parameters shown.'))),
          align = 'center')
 ),
 
 fluidRow(
   column(12, actionButton("go", strong("Go")), align = 'center'),
 ),
      
      tabsetPanel(tabPanel("Main Map", br(),  
                           
                           column(1),
                           
                           column(10, leafletOutput("int.map", height = 700)),
                           
                           column(1)),
                  
                  tabPanel("EPA Map",

                           fluidRow(h6(HTML('&#8287'))),
                           
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
                                                label = 'Map Layers',
                                                choices = list(
                                                  "PM\u2082.\u2085" = "pm25", 
                                                  "PM\u2081" = "pm1",
                                                  "PM\u2081\u2080" = "pm10",
                                                  "Temperature" = "temp",
                                                  "Humidity" = "humid",
                                                  "Crime" = "crime",
                                                  "Area Deprivation Index" = 'pov',
                                                  "Traffic" = 'tr',
                                                  "Tree Cover" = "tc",
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