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
        subset(Sensor.ID %in% c(input$sensors.hl, input$sensors.o))
      map.data <- subset(map.data, !is.na(map.data[,input$vars]))
      
      map.layer <- rasterize(map.data[,3:2], r, map.data[,input$vars], fun = mean) #Creates the bins image
      
      map.layer.t <- rasterize(map.data[,3:2], r, map.data$Temperature, fun = mean)
      map.layer.h <- rasterize(map.data[,3:2], r, map.data$Humidity, fun = mean)
      map.layer.pm1 <- rasterize(map.data[,3:2], r, map.data$PM1, fun = mean)
      map.layer.pm2.5 <- rasterize(map.data[,3:2], r, map.data$PM2.5, fun = mean)
      map.layer.pm10 <- rasterize(map.data[,3:2], r, map.data$PM10, fun = mean)
      map.layer.md <- rasterize(map.data[,3:2], r, map.data$Count, fun = sum)
      map.layer.mdl <- calc(map.layer.md, fun = function(x){log10(x)})
      
      for(i in 1:length(values(map.layer))){
        content[i] <- paste0(
          "Avg. temperature = ", round(values(map.layer.t)[i], digits = 1), "<br/>",
          "Avg. humidity = ", round(values(map.layer.h)[i], digits = 1), "<br/>",
          "Avg. PM1 = ", round(values(map.layer.pm1)[i], digits = 1), "<br/>",
          "Avg. PM2.5 = ", round(values(map.layer.pm2.5)[i], digits = 1), "<br/>",
          "Avg. PM10 = ", round(values(map.layer.pm10)[i], digits = 1), "<br/>",
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
    
    else if(input$vars == "PM2.5.d"){
      
      map.data <- app.data %>%
        subset(Day %in% input$dates[1]:input$dates[2]) %>%
        subset(Time %in% mins[grep(input$times[1], mins) : grep(input$times[2], mins)]) %>%
        subset(Sensor.ID %in% c(input$sensors.hl, input$sensors.o)) %>%
        subset(!is.na(PM2.5))
      
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
      
      map.layer <- rasterize(map.data[,3:2], r, map.data$PM2.5, fun = mean) #Creates the bins image
      map.layer <- f.discrete(map.layer)
      
      map.layer.t <- rasterize(map.data[,3:2], r, map.data$Temperature, fun = mean)
      map.layer.h <- rasterize(map.data[,3:2], r, map.data$Humidity, fun = mean)
      map.layer.pm1 <- rasterize(map.data[,3:2], r, map.data$PM1, fun = mean)
      map.layer.pm2.5 <- rasterize(map.data[,3:2], r, map.data$PM2.5, fun = mean)
      map.layer.pm10 <- rasterize(map.data[,3:2], r, map.data$PM10, fun = mean)
      map.layer.md <- rasterize(map.data[,3:2], r, map.data$Count, fun = sum)
      map.layer.mdl <- calc(map.layer.md, fun = function(x){log10(x)})
      
      for(i in 1:length(values(map.layer))){
        content[i] <- paste0(
          "Avg. temperature = ", round(values(map.layer.t)[i], digits = 1), "<br/>",
          "Avg. humidity = ", round(values(map.layer.h)[i], digits = 1), "<br/>",
          "Avg. PM1 = ", round(values(map.layer.pm1)[i], digits = 1), "<br/>",
          "Avg. PM2.5 = ", round(values(map.layer.pm2.5)[i], digits = 1), "<br/>",
          "Avg. PM10 = ", round(values(map.layer.pm10)[i], digits = 1), "<br/>",
          "# of ", f.units(input$vars), " data points = ", values(map.layer.md)[i]
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
                    title = "Avg. PM2.5 Conc. (\u03BCg/m\u00B3)", position = "topright", group = "Measurement value") %>%
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