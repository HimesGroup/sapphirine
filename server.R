
.libPaths("/home/maya/R/x86_64-pc-linux-gnu-library/3.4/") #mapview dependencies, use only for online version

source("global.R")

#Define server logic
server <- function(input, output, session){
  
  ## Plot map on GO -------------
  map.plot <- eventReactive(input$go, {
    
    #Creates base layer for raster layer to be added to map later
    r <- raster(nrows = input$row.no, ncols = input$col.no, xmn = input$lon.range[1], 
                xmx = input$lon.range[2], ymn = input$lat.range[1], ymx = input$lat.range[2])
    
    lons <- xFromCell(r, 1:ncell(r))
    lats <- yFromCell(r, 1:ncell(r))
    
    step.size.x <- (input$lon.range[2] - input$lon.range[1]) / input$col.no
    step.size.y <- (input$lat.range[2] - input$lat.range[1]) / input$row.no
    
    content <- vector()

    #Accounts for fact that time is subsetted by the hour 
    if(input$times[2] != "23:59"){
      upper.ind <- grep(input$times[2], mins) - 1
    }
    else{
      upper.ind <- grep("23:59", mins)
    }
     
    #Subsets data by user-selected date range and time-range
    #Removes rows containing NAs for selected measurement type
    map.data <- app.data %>%
      dplyr::filter(Day %in% input$dates[1]:input$dates[2],Time %in% mins[grep(input$times[1], mins) : upper.ind]) 
    
    sensor.data <- map.data %>% dplyr::filter(Sensor.ID %in% c(input$sensors.hl, input$sensors.o))
    
    # Remove big objects and clear memory -------------
    rm(map.data)

    ## Value map layers: -------------
    
    # Crime raster
    crime.data <- app.data %>% dplyr::filter(!is.na(Crime))
    assign("map.layer.c", rasterize(crime.data[,3:2], r, crime.data$Crime, fun = sum, na.rm = TRUE), 
           envir = .GlobalEnv)
    
    # Poverty raster
    assign("map.layer.pov", 
           try(resample(pov.raster, r, method = "bilinear"), silent = TRUE),
           envir = .GlobalEnv)
    if(length(map.layer.pov) == 1){
      assign("map.layer.pov", rasterize(data.frame(NA, NA), r, na.rm = TRUE), envir = .GlobalEnv)
    }
    
    # Traffic raster
    assign("map.layer.tr", 
           try(resample(traffic.raster, r, method = "bilinear"), silent = TRUE),
           envir = .GlobalEnv)
    if(length(map.layer.tr) == 1){
      assign("map.layer.tr", rasterize(data.frame(NA, NA), r, na.rm = TRUE), envir = .GlobalEnv)
    }
    
    # Value and density sensor rasters
    for(i in 1:length(sensor.measures)){
      suffix <- f.suffix(sensor.measures[i])
      measure.data <- subset(sensor.data, !is.na(sensor.data[,sensor.measures[i]]))
      if(nrow(measure.data) > 0){
        assign("density.raster", 
               rasterize(measure.data[,3:2], r, measure.data$Count, fun = sum, na.rm = TRUE))
        assign(paste0("map.layer", suffix),
               rasterize(measure.data[,3:2], r, measure.data[,sensor.measures[i]], fun = mean, na.rm = TRUE),
               envir = .GlobalEnv)
      }
      else{
        assign("density.raster",
               rasterize(data.frame(NA, NA), r, na.rm = TRUE))
        assign(paste0("map.layer", suffix),
               rasterize(data.frame(NA, NA), r, na.rm = TRUE),
               envir = .GlobalEnv)
      }       
      assign(paste0("map.layer", suffix, ".d"), density.raster, envir = .GlobalEnv)
      assign(paste0("map.layer", suffix, ".dlog"), 
             calc(density.raster, fun = function(x){log10(x)}), envir = .GlobalEnv)
    }
    
    ## Content format  -------------
    total_length <- seq(1,length(values(map.layer.pm2.5)))
    lat_lon <- vector()
    lat_lon <- paste0("<b>",
                      "Lat rng: [", "<b style = \"color:DimGray\">",gsub(" ", "", format(round(lats[total_length] - step.size.y/2, 5), nsmall = 5), fixed = TRUE), "</b>", ", ",
                      "<b style = \"color:DimGray\">",gsub(" ", "", format(round(lats[total_length] + step.size.y/2, 5), nsmall = 5), fixed = TRUE), "</b>", "]", "<br/>",
                      "Lon rng: [", "<b style = \"color:DimGray\">",gsub(" ", "", format(round(lons[total_length] - step.size.x/2, 5), nsmall = 5), fixed = TRUE), "</b>", ", ",
                      "<b style = \"color:DimGray\">",gsub(" ", "", format(round(lons[total_length] + step.size.x/2, 5), nsmall = 5), fixed = TRUE), "</b>", "]", "<br/>") 
    
    # Temperature
    templ <- vector()
    templ[which(!is.na(values(map.layer.t)))] <- paste0("Avg. temperature: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.t)[which(!is.na(values(map.layer.t)))], digits = 1),"\u00B0C", "</b>"," (", values(map.layer.t.d)[which(!is.na(values(map.layer.t)))], ")","<br/>")
    templ[which(is.na(values(map.layer.t)))] <- paste0("Avg. temperature: ","<b style = \"color:Tomato\">", "no data", "</b>", " (0)", "<br/>")
    
    
    # Humidity
    humidl <- vector()
    humidl[which(!is.na(values(map.layer.h)))] <- paste0("Avg. humidity: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.h)[which(!is.na(values(map.layer.h)))], digits = 1),"%", "</b>"," (", values(map.layer.h.d)[which(!is.na(values(map.layer.h)))], ")","<br/>")
    humidl[which(is.na(values(map.layer.h)))] <- paste0("Avg. humidity: ","<b style = \"color:Tomato\">", "no data", "</b>", " (0)", "<br/>")
    
    # PM1
    pm1l <- vector()
    pm1l[which(!is.na(values(map.layer.pm1)))] <- paste0("Avg. PM1: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.pm1)[which(!is.na(values(map.layer.pm1)))], digits = 2)," \u03BCg/m\u00B3", "</b>"," (", values(map.layer.pm1.d)[which(!is.na(values(map.layer.pm1)))], ")","<br/>")
    pm1l[which(is.na(values(map.layer.pm1)))] <- paste0("Avg. PM1: ","<b style = \"color:Tomato\">", "no data", "</b>", " (0)", "<br/>")
    
    # PM2.5l
    pm2.5l <- vector()
    pm2.5l[which(!is.na(values(map.layer.pm2.5)))] <- paste0("Avg. PM2.5: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.pm2.5)[which(!is.na(values(map.layer.pm2.5)))], digits = 2)," \u03BCg/m\u00B3", "</b>"," (", values(map.layer.pm2.5.d)[which(!is.na(values(map.layer.pm2.5)))], ")","<br/>")
    pm2.5l[which(is.na(values(map.layer.pm2.5)))] <- paste0("Avg. PM2.5: ","<b style = \"color:Tomato\">", "no data", "</b>", " (0)", "<br/>")
    
    # PM10l
    pm10l <- vector()
    pm10l[which(!is.na(values(map.layer.pm10)))] <- paste0("Avg. PM10: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.pm10)[which(!is.na(values(map.layer.pm10)))], digits = 2)," \u03BCg/m\u00B3", "</b>"," (", values(map.layer.pm10.d)[which(!is.na(values(map.layer.pm10)))], ")","<br/>")
    pm10l[which(is.na(values(map.layer.pm10)))] <- paste0("Avg. PM10: ","<b style = \"color:Tomato\">", "no data", "</b>", " (0)", "<br/>")
    
    # Crime
    crimel <- vector()
    cpoints <- point.in.polygon(xFromCell(map.layer.c, total_length), yFromCell(map.layer.c, total_length),city.border$Longitude, city.border$Latitude)
    
    # not NA with cpoint = 1
    indx1 <- intersect(which(!is.na(values(map.layer.c))),which(cpoints==1))
    crimel[indx1] <- paste0("# reported crimes: ","<b style = \"color:DodgerBlue\">", values(map.layer.c)[indx1], "</b>","<br/>")
    
    # NA with cpoint = 1
    indx2 <- intersect(which(is.na(values(map.layer.c))),which(cpoints==1))
    crimel[indx2] <- paste0("# reported crimes: ","<b style = \"color:DodgerBlue\">", "0", "</b>","<br/>")
    
    # not NA with cpoint != 1
    indx3 <- intersect(which(!is.na(values(map.layer.c))),which(cpoints!=1))
    crimel[indx3] <- paste0("# reported crimes: ","<b style = \"color:DodgerBlue\">", values(map.layer.c)[indx3], "</b>",
                            " (", "<b style = \"color:Tomato\">", "Phil. only", "</b>", ")","<br/>")
    
    # NA with cpoint != 1
    indx4 <- intersect(which(is.na(values(map.layer.c))),which(cpoints!=1))
    crimel[indx4] <- paste0("# reported crimes: ","<b style = \"color:Tomato\">", "no data", "</b>","<br/>")
    
    # Poverty
    povl <- vector()
    povl[which(!is.na(values(map.layer.pov)))] <- paste0("Avg. ADI: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.pov)[which(!is.na(values(map.layer.pov)))], digits = 2), "</b>","<br/>")
    povl[which(is.na(values(map.layer.pov)))] <- paste0("Avg. ADI: ","<b style = \"color:Tomato\">", "no data", "</b>", "<br/>")
    
    # Traffic
    trafl <- vector()
    tpoints <- point.in.polygon(xFromCell(map.layer.tr, total_length), yFromCell(map.layer.tr, total_length),city.border$Longitude, city.border$Latitude)
    
    # NA
    trafl[which(is.na(values(map.layer.tr)))] <- paste0("Avg. AADT: ","<b style = \"color:Tomato\">", "no data", "</b>","<br/>","</b>")
    
    # tpoint = 1 and not NA
    indt1 <- intersect(which(!is.na(values(map.layer.tr))),which(tpoints==1))
    trafl[indt1] <- paste0("Avg. AADT: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.tr)[indt1], digits = 0), "</b>","<br/>","</b>")
    
    # tpoint != 1 and not NA
    indt2 <- intersect(which(!is.na(values(map.layer.tr))),which(tpoints!=1))
    trafl[indt2] <- paste0("Avg. AADT: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.tr)[indt2], digits = 0), "</b>",
                           " (", "<b style = \"color:Tomato\">", "Phil. only", "</b>", ")","<br/>","</b>")
    
    ## Final content vector -------------
    content <- paste0(lat_lon,templ,humidl,pm1l,pm2.5l,pm10l,crimel,povl,trafl)
    
    #Remove big objects -------------
    rm(lat_lon,templ,humidl,pm1l,pm2.5l,pm10l,crimel,povl,trafl)
    
    #Indicies for removing popups with all NA
    inds.df <- cbind(values(map.layer.t),
                     values(map.layer.h),
                     values(map.layer.pm1),
                     values(map.layer.pm2.5),
                     values(map.layer.pm10),
                     values(map.layer.c),
                     values(map.layer.pov),
                     values(map.layer.tr)
                     )
    
    inds <- apply(inds.df, 1, function(x) all(is.na(x)))
    
    #Removes popups for which all data are NA
    #Coercing lat and lon to NA works better than removing these rows
    content.df <- data.frame(cbind(lons, lats, content), stringsAsFactors = FALSE)
    content.df[,1:2] <- sapply(content.df[,1:2], as.numeric)
    content.df[inds, 1:2] <- NA

    colors <- brewer.pal(7, "YlOrRd")
    colors.d <- brewer.pal(7, "Purples")
    
    for(i in 1:length(all.measures)){
      suffix <- f.suffix(all.measures[i])
      vals <- values(eval(parse(text = paste0("map.layer", suffix))))
      if(!all(is.na(vals))){
        vals <- c(0, vals, f.top(max(vals, na.rm = TRUE)))
        assign(paste0("pal", suffix), 
               colorNumeric(palette = colors, 
                            domain = vals, 
                            na.color = "transparent"),
               envir = .GlobalEnv)
        assign(paste0("leg.pal", suffix), 
               colorNumeric(palette = colors, 
                            domain = vals, 
                            na.color = "transparent",
                            reverse = TRUE),
               envir = .GlobalEnv)
      }
      else{
        assign(paste0("pal", suffix),
               colorNumeric(palette = colors,
                            domain = 0,
                            na.color = "transparent"),
               envir = .GlobalEnv)
        assign(paste0("leg.pal", suffix), 
               colorNumeric(palette = colors, 
                            domain = 0, 
                            na.color = "transparent",
                            reverse = TRUE),
               envir = .GlobalEnv)
      }
    }
    
    for(i in 1:length(sensor.measures)){
      suffix <- f.suffix(sensor.measures[i])
      vals <- values(eval(parse(text = paste0("map.layer", suffix, ".dlog"))))
      if(!all(is.na(vals))){
        vals <- c(0, vals, f.top(max(vals, na.rm = TRUE)))
        assign(paste0("pal", suffix, ".d"),
               colorNumeric(palette = colors.d, 
                            domain = vals, 
                            na.color = "transparent"),
               envir = .GlobalEnv)
        assign(paste0("leg.pal", suffix, ".d"), 
               colorNumeric(palette = colors.d, 
                            domain = vals, 
                            na.color = "transparent",
                            reverse = TRUE),
               envir = .GlobalEnv)
      }
      else{
        assign(paste0("pal", suffix, ".d"),
               colorNumeric(palette = colors.d,
                            domain = 0,
                            na.color = "transparent"),
               envir = .GlobalEnv)
        assign(paste0("leg.pal", suffix, ".d"), 
               colorNumeric(palette = colors.d, 
                            domain = 0, 
                            na.color = "transparent",
                            reverse = TRUE),
               envir = .GlobalEnv)
      }
    }
    
    lon.center <- (input$lon.range[2] + input$lon.range[1]) / 2
    lat.center <- (input$lat.range[2] + input$lat.range[1]) / 2
    zoom.no <- f.zoom(input$lon.range[2] - input$lon.range[1], input$lat.range[2] - input$lat.range[1])
    button.js <- paste0("function(btn, map){ map.setView([", lat.center, ", ", lon.center, "], ", zoom.no, "); }")
    
    vals <- values(map.layer.pm2.5)
    if(!all(is.na(vals))){
      vals <- c(0, vals, f.top(max(vals, na.rm = TRUE)))
    }

    vals.d <- values(map.layer.pm2.5.dlog)
    if(!all(is.na(vals))){
      vals.d <- c(0, vals.d, f.top(max(vals.d, na.rm = TRUE)))
    }
    
    ## Initialize leaflet -------------
    leaflet(content.df) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(map.layer.pm2.5, colors = pal.pm2.5, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
      addLegend(pal = leg.pal.pm2.5, values = vals, opacity = 1,
                title = toString(f.titles("PM2.5")), position = "topright",
                group = "Measurement value",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addRasterImage(map.layer.pm2.5.dlog, colors = pal.pm2.5.d, opacity = 0.8, group = "Measurement density", method = "ngb") %>%
      addLegend(pal = leg.pal.pm2.5.d, values = vals.d, opacity = 1, 
                title = paste("log # of PM2.5 data points"),
                group = "Measurement density", position = "topright",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addCircleMarkers(~lons, ~lats, popup = ~content, stroke = FALSE, fillOpacity = 0.001) %>%
      addMeasure(position = "topleft", primaryLengthUnit = "meters", secondaryLengthUnit = "miles",
                 primaryAreaUnit = "sqmeters", secondaryAreaUnit = "sqmiles") %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Recenter",
        onClick = JS(paste(button.js))
      )) %>%
      leafem::addMouseCoordinates() %>%
      addLayersControl(baseGroups = all.measures, 
                       overlayGroups = c("Measurement value", "Measurement density"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      showGroup(c("PM2.5", "Measurement value")) %>%
      hideGroup(c(all.measures[which(all.measures != "PM2.5")], "Measurement density"))

  }) #End eventReactive
  
  #Delays plotting until "Go" button is clicked
  
  observeEvent(input$go, {
    output$int.map <- renderLeaflet({
      withProgress(message = "Loading map...", {map.plot()})
    })
  })
  
  ## Reactive : Type of measurement selected -------------
  measure <- reactive({input$int.map_groups[[2]]})
  
  observeEvent(input$int.map_groups, {
    
    map <- leafletProxy("int.map", session) %>%
      clearControls() %>%
      removeMarker(c("null1", "null2", "null3")) %>%
      clearPopups()
    
    ## Measurement value selected -------------
    if("Measurement value" %in% input$int.map_groups && !"Measurement density" %in% input$int.map_groups){
      
      suffix <- f.suffix(measure())
      map.layer <- eval(parse(text = paste0("map.layer", suffix)))
      legend.title <- toString(f.titles(measure()))
      
      if(measure() %in% all.measures){
        
        if(all(is.na(values(map.layer)))){
          map %>%
            clearImages() %>%
            addLabelOnlyMarkers(
              lng = -75.15, lat = 40.00,
              label = "No data",
              layerId = "null1",
              labelOptions = labelOptions(noHide = TRUE,
                                          style = list(
                                            "color" = "red",
                                            "font-size" = "20px",
                                            "font-family" = "serif",
                                            "border-color" = "rgba(0,0,0,1)"
                                          )))
        }
        
        else{
          
          pal <- eval(parse(text = paste0("pal", suffix)))
          leg.pal <- eval(parse(text = paste0("leg.pal", suffix)))
          vals <- values(map.layer)
          vals <- c(0, vals, f.top(max(vals, na.rm = TRUE)))
          
          map %>%
            clearImages() %>%
            removeMarker("null1") %>%
            addRasterImage(map.layer, colors = pal, opacity = 0.8, method = "ngb") %>%
            addLegend(pal = leg.pal, values = vals, opacity = 1,
                      title = legend.title, position = "topright",
                      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
            showGroup(c(input$int.map_groups, "Measurement value")) %>%
            hideGroup(c(all.measures[which(all.measures != measure())], "Measurement density"))
        }
      }
      #}
    }
    
    ## Measurement density selected -------------
    else if("Measurement density" %in% input$int.map_groups && !"Measurement value" %in% input$int.map_groups){
      
      if(measure() %in% sensor.measures){
        
        suffix <- f.suffix(measure())
        map.layer <- eval(parse(text = paste0("map.layer", suffix, ".dlog")))
        legend.title <- toString(f.titles.d(measure()))
        
        if(all(is.na(values(map.layer)))){
          map %>%
            clearImages() %>%
            addLabelOnlyMarkers(
              lng = -75.15, lat = 40.00,
              label = "No data",
              layerId = "null1",
              labelOptions = labelOptions(noHide = TRUE,
                                          style = list(
                                            "color" = "red",
                                            "font-size" = "20px",
                                            "font-family" = "serif",
                                            "border-color" = "rgba(0,0,0,1)"
                                          )))
        }
        
        else{
          
          pal <- eval(parse(text = paste0("pal", suffix, ".d")))
          leg.pal <- eval(parse(text = paste0("leg.pal", suffix, ".d")))
          vals <- values(map.layer)
          vals <- c(0, vals, f.top(max(vals, na.rm = TRUE)))
          
          map %>%
            clearImages() %>%
            removeMarker("null1") %>%
            addRasterImage(map.layer, colors = pal, opacity = 0.8, method = "ngb") %>%
            addLegend(pal = leg.pal, values = vals, opacity = 1,
                      title = legend.title, position = "topright",
                      labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
            showGroup(c(measure(), "Measurement density")) %>%
            hideGroup(c(all.measures[which(all.measures != measure())], "Measurement value"))
        }
        
        # Check for crime, poverty and traffic -------------
      } else if(measure() %in% other.measures){ 
        map %>%
          clearImages() %>%
          addLabelOnlyMarkers(
            lng = -75.15, lat = 40.00,
            label = "N/A",
            layerId = "null2",
            labelOptions = labelOptions(noHide = TRUE,
                                        style = list(
                                          "color" = "red",
                                          "font-size" = "20px",
                                          "font-family" = "serif",
                                          "border-color" = "rgba(0,0,0,1)"
                                        )))
      }
      
    }
    # Both value and density selected -------------
    else if("Measurement value" %in% input$int.map_groups && "Measurement density" %in% input$int.map_groups){
      map %>%
        clearImages() %>%
        addLabelOnlyMarkers(
          lng = -75.15, lat = 40.00,
          label = "Please select only \"Measurement value\" or \"Measurement density\".",
          layerId = "null3",
          labelOptions = labelOptions(noHide = TRUE,
                                      style = list(
                                        "color" = "red",
                                        "font-size" = "20px",
                                        "font-family" = "serif",
                                        "border-color" = "rgba(0,0,0,1)"
                                      )))
    }
    # None selected -------------
    else if(!"Measurement value" %in% input$int.map_groups && !"Measurement density" %in% input$int.map_groups){
      map %>%
        clearImages() %>%
        addLabelOnlyMarkers(
          lng = -75.15, lat = 40.00,
          label = "Please select either \"Measurement value\" or \"Measurement density\".",
          layerId = "null3",
          labelOptions = labelOptions(noHide = TRUE,
                                      style = list(
                                        "color" = "red",
                                        "font-size" = "20px",
                                        "font-family" = "serif",
                                        "border-color" = "rgba(0,0,0,1)"
                                      )))
    }
    
  })
  
}#End server function