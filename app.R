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

app.data <- read.csv("./databases/all_data.csv", header = TRUE)

app.data$Timestamp <- force_tz(ymd_hms(app.data$Timestamp), tz = "America/New_York") #Reads in timestamp as integer, so converts to POSIXct

app.data <- mutate(app.data, Day = date(Timestamp), Time = strftime(Timestamp, format = "%H:%M"), Count = 1) 
#Creates two new columns for later subsetting of data by date and by time, as well as one column for measurement density count

mins <- unique(strftime(force_tz(as_datetime(ymd_hm("1970-01-01 00:00") : ymd_hm("1970-01-01 23:59")), tz = "America/New_York"), format = "%H:%M"))
mins.5 <- unique(strftime(floor_date(force_tz(as_datetime(ymd_hm("1970-01-01 00:00") : ymd_hm("1970-01-01 23:59")), tz = "America/New_York"), unit = "5 min"), format = "%H:%M"))
mins.5 <- c(mins.5, "23:59")
#Creates 2 lists of times from 0:00 to 23:59, one by the minute and one by 5 minute intervals, given as strings
#5 minute intervals will be used on slider
#1 minute interval list will be indexed by a "grep" function using the 5 minute interval choices

assign("df.list.2", as.list(read.csv("./databases/df_list.csv", stringsAsFactors = FALSE)[1,]), envir = .GlobalEnv)

titles.list <- list("Avg. Humidity (%)", "Avg. Temp. (\u00B0C)", "Avg. PM-2.5 Conc. (\u03BCg/m\u00B3)", "Avg. NO Conc. (ppb)", 
                    "Avg. Barometric Pressure (hPa)", "Avg. CO Conc. (ppm)", "Avg. CO\u2082 Conc. (ppm)")

abbrs.list <- list("Humidity", "Temp.", "PM-2.5", "NO", "Pressure", "CO", "CO\u2082")

titles.df <- data.frame(cbind(df.list.2, titles.list, abbrs.list))

f.titles <- function(y){
  
  if(y %in% df.list.2){  
    index <- grep(paste(y), titles.df[,1])
    return(titles.df[index, 2])
  }
  
  else if(length(grep(".mdb", y)) > 0){
    index <- grep(gsub(".mdb", "", y), titles.df[,1])
    return(paste("log # of", titles.df[index, 3], "Data Points"))
  }
  
} #End f.titles

f.units <- function(z){
  index <- grep(paste(z), titles.df[,1])
  return(paste(titles.df[index, 3]))
}

our.sensors <- read.csv("./databases/LIMEA_AIRBEAM_SUMMARY.csv", header = TRUE, stringsAsFactors = FALSE)$AirBeamID[1:15]
our.sensors <- paste0("AirBeam:", our.sensors)

sensor.names <- names(table(app.data$Sensor_ID))

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
                           "Particulate matter - continuous" = "ParticulateMatter",
                           "Particulate matter - discrete" = "ParticulateMatter.d",
                           "Temperature" = "Temperature",
                           "Humidity" = "Humidity",
                           "Barometric pressure" = "Barometricpressure",
                           "CO concentration" = "COconcentration",
                           "CO\u2082 concentration" = "CO2concentration",
                           "NO concentration" = "NOconcentration"
                         ),
                         multiple = FALSE,
                         selected = "ParticulateMatter"
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


#Define server logic
server <- function(input, output){
  
  map.plot <- eventReactive(input$go, {
    
    #Creates base layer for raster layer to be added to map later
    r <- raster(nrows = input$bin.no, ncols = input$bin.no, xmn = -75.25, xmx = -75.05, ymn = 39.90, ymx = 40.10)
    
    step.size.x <- ((-75.05) - (-75.25)) / input$bin.no
    step.size.y <- (40.10 - 39.90) / input$bin.no
    lons <- rep(seq(-75.25 + step.size.x / 2, -75.05 - step.size.x / 2, step.size.x), input$bin.no)
    lats <- rep(seq(40.10 - step.size.y / 2, 39.90 + step.size.y / 2, -step.size.y), each = input$bin.no)
    content <- vector()
    
    if(input$vars %in% df.list.2){ #Convenient way to select variable names for "value" plot type
      
      #Subsets data by user-selected date range, time range, and measurement type
      #Removes rows containing NAs for selected measurement type
      map.data <- app.data %>%
        subset(Day %in% input$dates[1]:input$dates[2]) %>%
        subset(Time %in% mins[grep(input$times[1], mins) : grep(input$times[2], mins)]) %>%
        subset(Sensor_ID %in% c(input$sensors.hl, input$sensors.o))
      map.data <- subset(map.data, !is.na(map.data[,input$vars]))
      
      map.layer <- rasterize(map.data[,2:3], r, map.data[,input$vars], fun = mean) #Creates the bins image
      
      map.layer.pm <- rasterize(map.data[,2:3], r, map.data$ParticulateMatter, fun = mean)
      map.layer.t <- rasterize(map.data[,2:3], r, map.data$Temperature, fun = mean)
      map.layer.h <- rasterize(map.data[,2:3], r, map.data$Humidity, fun = mean)
      map.layer.md <- rasterize(map.data[,2:3], r, map.data$Count, fun = sum)
      map.layer.mdl <- calc(map.layer.md, fun = function(x){log10(x)})
      
      for(i in 1:length(values(map.layer))){
        content[i] <- paste0(
          "Avg. temperature = ", round(values(map.layer.t)[i], digits = 1), "<br/>",
          "Avg. humidity = ", round(values(map.layer.h)[i], digits = 1), "<br/>",
          "Avg. PM-2.5 = ", round(values(map.layer.pm)[i], digits = 1), "<br/>",
          "# of ", f.units(input$vars), " data points = ", values(map.layer.md)[i]
        )
      }
      
      inds <- row.names(subset(data.frame(values(map.layer)), !is.na(values(map.layer)))) #Indicies for popup removal below
      
      content.df <- data.frame(cbind(lons, lats, content), stringsAsFactors = FALSE)
      content.df[,1:2] <- sapply(content.df[,1:2], as.numeric)
      content.df <- content.df[inds,] #Removes popups with "NA" for the selected variable
      
      pal <- colorNumeric(palette = "BuPu", domain = values(map.layer), na.color = "transparent")
      pal2 <- colorNumeric(palette = "BuPu", domain = values(map.layer.mdl), na.color = "transparent")
      
      if(input$d.type == "mv"){
        leaflet(content.df) %>%
          setView(lng = -75.15, lat = 40.00, zoom = 12) %>%
          addTiles(options = providerTileOptions(minZoom = 9, maxZoom = 18)) %>%
          addRasterImage(map.layer, colors = "BuPu", opacity = 0.8) %>%
          addLegend(pal = pal, values = values(map.layer), opacity = 1,
                    title = toString(f.titles(input$vars)), position = "topright") %>%
          addCircleMarkers(~lons, ~lats, popup = ~content, stroke = FALSE, fillOpacity = 0.001) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS("function(btn, map){ map.setView([40.00, -75.15], 12); }")
          ))
      }
      
      else if(input$d.type == "md"){
        leaflet(content.df) %>%
          setView(lng = -75.15, lat = 40.00, zoom = 12) %>%
          addTiles(options = providerTileOptions(minZoom = 9, maxZoom = 18)) %>%
          addCircleMarkers(~lons, ~lats, popup = ~content, stroke = FALSE, fillOpacity = 0.001) %>%
          addRasterImage(map.layer.mdl, colors = "BuPu", opacity = 0.8, group = "Measurement density") %>%
          addLegend(pal = pal2, values = values(map.layer.mdl), opacity = 1, title = paste("log # of", f.units(input$vars), "data points"),
                    group = "Measurement density", position = "topright") %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS("function(btn, map){ map.setView([40.00, -75.15], 12); }")
          ))
      }
      
    } #End "if in df.list.2" statement
    
    else if(input$vars == "ParticulateMatter.d"){
      
      map.data <- app.data %>%
        subset(Day %in% input$dates[1]:input$dates[2]) %>%
        subset(Time %in% mins[grep(input$times[1], mins) : grep(input$times[2], mins)]) %>%
        subset(Sensor_ID %in% c(input$sensors.hl, input$sensors.o)) %>%
        subset(!is.na(ParticulateMatter))
      
      f.discrete <- function(x){
        for(i in 1:length(values(x))){
          if(!is.na(values(x)[i])){
            if(values(x)[i] >= 0 && values(x)[i] < 12){values(x)[i] <- 1}
            if(values(x)[i] >= 12 && values(x)[i] < 35){values(x)[i] <- 2}
            if(values(x)[i] >= 35 && values(x)[i] < 55){values(x)[i] <- 3}
            if(values(x)[i] >= 55){values(x)[i] <- 4}
          }
        }
        return(x)
      }
      
      map.layer <- rasterize(map.data[,2:3], r, map.data$ParticulateMatter, fun = mean) #Creates the bins image
      map.layer <- f.discrete(map.layer)
      
      map.layer.pm <- rasterize(map.data[,2:3], r, map.data$ParticulateMatter, fun = mean)
      map.layer.t <- rasterize(map.data[,2:3], r, map.data$Temperature, fun = mean)
      map.layer.h <- rasterize(map.data[,2:3], r, map.data$Humidity, fun = mean)
      map.layer.md <- rasterize(map.data[,2:3], r, map.data$Count, fun = sum)
      map.layer.mdl <- calc(map.layer.md, fun = function(x){log10(x)})
      
      for(i in 1:length(values(map.layer))){
        content[i] <- paste0(
          "Avg. temperature = ", round(values(map.layer.t)[i], digits = 1), "<br/>",
          "Avg. humidity = ", round(values(map.layer.h)[i], digits = 1), "<br/>",
          "Avg. PM-2.5 = ", round(values(map.layer.pm)[i], digits = 1), "<br/>",
          "# of PM-2.5 data points = ", values(map.layer.md)[i]
        )
      }
      
      inds <- row.names(subset(data.frame(values(map.layer)), !is.na(values(map.layer)))) #Indicies for popup removal below
      
      content.df <- data.frame(cbind(lons, lats, content), stringsAsFactors = FALSE)
      content.df[,1:2] <- sapply(content.df[,1:2], as.numeric)
      content.df <- content.df[inds,] #Removes popups with "NA" for the selected variable
      
      pal2 <- colorNumeric(palette = "BuPu", domain = values(map.layer.mdl), na.color = "transparent")
      
      if(input$d.type == "mv"){
        leaflet(content.df) %>%
          setView(lng = -75.15, lat = 40.00, zoom = 12) %>%
          addTiles(options = providerTileOptions(minZoom = 9, maxZoom = 18)) %>%
          addRasterImage(map.layer, colors = c("#65C68A", "#FEE665", "#FEB065", "#FE6465"), opacity = 0.8, group = "Measurement value") %>%
          addLegend(colors = c("#65C68A", "#FEE665", "#FEB065", "#FE6465"), labels = c("0-12", "12-35", "35-55", "55+"), opacity = 0.8,
                    title = "Avg. PM-2.5 Conc. (\u03BCg/m\u00B3)", position = "topright", group = "Measurement value") %>%
          addCircleMarkers(~lons, ~lats, popup = ~content, stroke = FALSE, fillOpacity = 0.001) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS("function(btn, map){ map.setView([40.00, -75.15], 12); }")
          ))
      }
      
      else if(input$d.type == "md"){
        leaflet(content.df) %>%
          setView(lng = -75.15, lat = 40.00, zoom = 12) %>%
          addTiles(options = providerTileOptions(minZoom = 9, maxZoom = 18)) %>%
          addCircleMarkers(~lons, ~lats, popup = ~content, stroke = FALSE, fillOpacity = 0.001) %>%
          addRasterImage(map.layer.mdl, colors = "BuPu", opacity = 0.8, group = "Measurement density") %>%
          addLegend(pal = pal2, values = values(map.layer.mdl), opacity = 1, title = paste("log # of PM-2.5 data points"),
                    group = "Measurement density", position = "topright") %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS("function(btn, map){ map.setView([40.00, -75.15], 12); }")
          ))
      }
      
    } #End "if particulate matter discrete" statement
  
  }) #End eventReactive
  
  #Delays plotting until "Go" button is clicked
  observeEvent(input$go, {
               output$int.map <- renderLeaflet({map.plot()})
               })
  
  observeEvent(input$ss, {
    mapshot(map.plot(), file = paste0("./map_captures/", toString(Sys.Date()), "_", strftime(Sys.time(), format = "%H%M%S"), ".png"))
  })
  
  
}#End server function

#Run the app
shinyApp(ui = ui, server = server)
