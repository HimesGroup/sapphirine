
source("global.R")

#Define server logic
server <- function(input, output, session){
  
  observeEvent(input$Instructions_b1, {
    toggle("Instructions_1")
  })
  
  observeEvent(input$Instructions_b2, {
    toggle("Instructions_2")
  })
  
  
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
      dplyr::filter(
        date(Timestamp) %in% input$dates[1]:input$dates[2],
        strftime(Timestamp, format = '%H:%M', tz = 'America/New_York') %in% 
          mins[grep(input$times[1], mins) : upper.ind])
    
    sensor.data <- map.data %>% dplyr::filter(Sensor.ID %in% c(input$sensors.hl, input$sensors.o))
    
    assign('download.data', subset(sensor.data, 
                                   select = -Count), 
           envir = .GlobalEnv)
    
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
      measure.data <- dplyr::filter(sensor.data, !is.na(eval(parse(text = sensor.measures[i]))))
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
    pm1l[which(!is.na(values(map.layer.pm1)))] <- paste0("Avg. PM<sub>1</sub>: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.pm1)[which(!is.na(values(map.layer.pm1)))], digits = 2)," \u03BCg/m\u00B3", "</b>"," (", values(map.layer.pm1.d)[which(!is.na(values(map.layer.pm1)))], ")","<br/>")
    pm1l[which(is.na(values(map.layer.pm1)))] <- paste0("Avg. PM<sub>1</sub>: ","<b style = \"color:Tomato\">", "no data", "</b>", " (0)", "<br/>")
    
    # PM2.5l
    pm2.5l <- vector()
    pm2.5l[which(!is.na(values(map.layer.pm2.5)))] <- paste0("Avg. PM<sub>2.5</sub>: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.pm2.5)[which(!is.na(values(map.layer.pm2.5)))], digits = 2)," \u03BCg/m\u00B3", "</b>"," (", values(map.layer.pm2.5.d)[which(!is.na(values(map.layer.pm2.5)))], ")","<br/>")
    pm2.5l[which(is.na(values(map.layer.pm2.5)))] <- paste0("Avg. PM<sub>2.5</sub>: ","<b style = \"color:Tomato\">", "no data", "</b>", " (0)", "<br/>")
    
    # PM10l
    pm10l <- vector()
    pm10l[which(!is.na(values(map.layer.pm10)))] <- paste0("Avg. PM<sub>10</sub>: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.pm10)[which(!is.na(values(map.layer.pm10)))], digits = 2)," \u03BCg/m\u00B3", "</b>"," (", values(map.layer.pm10.d)[which(!is.na(values(map.layer.pm10)))], ")","<br/>")
    pm10l[which(is.na(values(map.layer.pm10)))] <- paste0("Avg. PM<sub>10</sub>: ","<b style = \"color:Tomato\">", "no data", "</b>", " (0)", "<br/>")
    
    # Crime
    crimel <- vector()
    cpoints <- point.in.SpatialPolygons(
      xFromCell(map.layer.c, total_length), 
      yFromCell(map.layer.c, total_length),
      city.border)
    
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
    tpoints <- point.in.SpatialPolygons(
      xFromCell(map.layer.tr, total_length), 
      yFromCell(map.layer.tr, total_length),
      city.border)
    
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
    rm(inds.df)
    
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
      } else{
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
    content_map <- leaflet(content.df) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(map.layer.pm2.5, colors = pal.pm2.5, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
      addLegend(pal = leg.pal.pm2.5, values = vals, opacity = 1,
                title = toString(f.titles("PM2.5")), position = "topright",
                group = "Measurement value",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addRasterImage(map.layer.pm2.5.dlog, colors = pal.pm2.5.d, opacity = 0.8, group = "Measurement density", method = "ngb") %>%
      addLegend(pal = leg.pal.pm2.5.d, values = vals.d, opacity = 1, 
                title = paste("log\u2081\u2080 # of PM<sub>2.5</sub> data points"),
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
      addLayersControl(baseGroups = all.measures.sub, 
                       overlayGroups = c("Measurement value", "Measurement density"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      showGroup(c("PM\u2082.\u2085", "Measurement value")) %>%
      hideGroup(c(all.measures.sub[which(all.measures.sub != "PM\u2082.\u2085")],
                  "Measurement density"))
##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###    
    #Plot individual maps side-by-side in a grid
    #Make PM2.5 maps
    pm25 <- leaflet(content.df) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(map.layer.pm2.5, colors = pal.pm2.5, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
      addLegend(pal = leg.pal.pm2.5, values = vals, opacity = 1,
                title = toString(f.titles("PM2.5")), position = "topright",
                group = "Measurement value",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    
    #Make PM1 maps
    vals1 <- values(map.layer.pm1)
    pm1 <- leaflet(content.df) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(map.layer.pm1, colors = pal.pm1, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
      addLegend(pal = leg.pal.pm1, values = vals1, opacity = 1,
                title = toString(f.titles("PM1")), position = "topright",
                group = "Measurement value",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    
    #Make PM10 maps
    vals10 <- values(map.layer.pm10)
    pm10 <- leaflet(content.df) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(map.layer.pm10, colors = pal.pm10, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
      addLegend(pal = leg.pal.pm10, values = vals10, opacity = 1,
                title = toString(f.titles("PM10")), position = "topright",
                group = "Measurement value",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    
    #Make temperature maps
    valst <- values(map.layer.t)
    temp <- leaflet(content.df) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(map.layer.t, colors = pal.t, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
      addLegend(pal = leg.pal.t, values = valst, opacity = 1,
                title = toString(f.titles("Temperature")), position = "topright",
                group = "Measurement value",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    
    #Make humidity maps
    valsh <- values(map.layer.h)
    humid <- leaflet(content.df) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(map.layer.h, colors = pal.h, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
      addLegend(pal = leg.pal.h, values = valsh, opacity = 1,
                title = toString(f.titles("Humidity")), position = "topright",
                group = "Measurement value",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    
    #Make crime maps
    valsc <- values(map.layer.c)
    crime <- leaflet(content.df) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(map.layer.c, colors = pal.c, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
      addLegend(pal = leg.pal.c, values = valsc, opacity = 1,
                title = toString(f.titles("Crime")), position = "topright",
                group = "Measurement value",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    
    #Make poverty maps
    valspov <- values(map.layer.pov)
    pov <- leaflet(content.df) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(map.layer.pov, colors = pal.pov, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
      addLegend(pal = leg.pal.pov, values = valspov, opacity = 1,
                title = toString(f.titles("Poverty")), position = "topright",
                group = "Measurement value",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    
    #Make traffic maps
    valstr <- values(map.layer.tr)
    tr <- leaflet(content.df) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(map.layer.tr, colors = pal.tr, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
      addLegend(pal = leg.pal.tr, values = valstr, opacity = 1,
                title = toString(f.titles("Traffic")), position = "topright",
                group = "Measurement value",
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))
    
    #make list of all maps
    maps <- list(main = content_map, pm25=pm25, pm1=pm1, pm10=pm10,temp=temp,humid=humid,crime=crime, pov=pov)
    maps

  }) #End eventReactive
  
  #Delays plotting until "Go" button is clicked
  
  observeEvent(input$go, {
    output$int.map <- renderLeaflet({
      withProgress(message = "Loading map...", {
        maps <- map.plot()
        #main UI map
        maps$main})
    })
  })
  
  #Initialize 
  all_maps <- reactiveValues(dat = 0)
  
  #Plot individual maps side-by-side in a grid
  observeEvent(input$go, {
    output$all.maps <- renderUI({
      withProgress(message = "Loading map...", {
        maps <- map.plot()
        #main UI map
        all_maps$dat <- sync(maps$pm25, maps$pm1,maps$pm10,maps$temp,maps$humid, maps$crime, maps$pov)})
    })
  })
 
  #Download individual maps side-by-side in a grid
  output$all_maps_download <- downloadHandler(
    filename = "all_maps.png",
    content = function(file) {
      mapshot(all_maps$dat, file = file)
    }
  )
##>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>###   
  
  ## Reactive : Type of measurement selected -------------
  measure <- reactive({input$int.map_groups[[2]]})
  
  observeEvent(input$int.map_groups, {
    
    map <- leafletProxy("int.map", session) %>%
      clearControls() %>%
      removeMarker(c("null1", "null2", "null3")) %>%
      clearPopups()
    
    ## Measurement value selected -------------
    if("Measurement value" %in% input$int.map_groups && !"Measurement density" %in% input$int.map_groups){
      
      meas.text <- f.plaintext(measure())
      suffix <- f.suffix(meas.text)
      map.layer <- eval(parse(text = paste0("map.layer", suffix)))
      legend.title <- toString(f.titles(meas.text))
      
      if(measure() %in% all.measures.sub){
        
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
            hideGroup(c(all.measures.sub[which(all.measures.sub != measure())], "Measurement density"))
        }
      }
      #}
    }
    
    ## Measurement density selected -------------
    else if("Measurement density" %in% input$int.map_groups && !"Measurement value" %in% input$int.map_groups){
      
      if(measure() %in% sensor.measures.sub){
        
        meas.text <- f.plaintext(measure())
        suffix <- f.suffix(meas.text)
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
            hideGroup(c(all.measures.sub[which(all.measures.sub != measure())], 
                        "Measurement value"))
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
    
  }) #End observeEvent for switching between variables in map
  
  #EPA
  epa.plot <- eventReactive(input$EPA_go, {
    
    epa.dates <- input$EPA_dates[1]:input$EPA_dates[2]
    
    col.name <- names(EPA_data)[grep(input$EPA_var, names(EPA_data))]

    epa.ras <- getEPAraster(input$EPA_var, epa.dates) #Also returns epa.df for downloading data
    epa.ras[epa.ras == -1] <- NA #Non-values are given as -1 by raster function
    epa.ras <- crop(epa.ras, extent(county.borders)) %>% mask(county.borders)
    
    mon.frame <- epa.df %>%
      dplyr::select(Longitude, Latitude, AQS_Site_ID, Local.Site.Name,
             State.Name, City.Name, Monitor_Start_Date, Last_Sample_Date) %>%
      unique()
    #Gives just monitor stats (removes repetition from dates, variables and measurements)
    
    epa.df.name <- paste('EPA', input$EPA_var, sep = '_')
    
    assign('epa.df.name', epa.df.name, envir = .GlobalEnv)
    
    #Give correct values for start and end of EPA sampling range
    mon.frame$Monitor_Start_Date[which(
      mon.frame$Monitor_Start_Date < input$EPA_dates[1])] <- input$EPA_dates[1]
    mon.frame$Last_Sample_Date[which(
      mon.frame$Last_Sample_Date > input$EPA_dates[2])] <- input$EPA_dates[2]
    
    vals <- values(epa.ras)
    if(all(is.na(vals))){vals <- c(0, vals)}
    
    #Pop-up content
    ## Content format  -------------
    
    site <- paste0("<b>",
                    "Site Name: ", "<b style = \"color:DodgerBlue\">", mon.frame$Local.Site.Name, "</b>", "<br/>")
    
    id <- paste0("AQS Site ID: ", "<b style = \"color:DodgerBlue\">", mon.frame$AQS_Site_ID, "</b>", "<br/>")
    
    city_state <- paste0("Location: ", "<b style = \"color:SeaGreen\">", mon.frame$City.Name, 
                         ", ", mon.frame$State.Name, "</b>", "<br/>")
    
    lat_lon <- paste0("Latitude: ", "<b style = \"color:DimGray\">", mon.frame$Latitude, "</b>", ", ",
                      "Longitude: ", "<b style = \"color:DimGray\">", mon.frame$Longitude, "</b>", "<br/>",
                      "</b>")
    
    avg.range <- paste0("Averaged over ", mon.frame$Monitor_Start_Date, 
                        " \u2013 ", mon.frame$Last_Sample_Date)
    avg.range[grep('NA', avg.range)] <-
      paste0("Measurement duration not available")
    
    
    lons <- mon.frame$Longitude
    lats <- mon.frame$Latitude
    
    epa.lons <- xFromCell(epa.ras, 1:ncell(epa.ras))
    epa.lats <- yFromCell(epa.ras, 1:ncell(epa.ras))
    
    #Popup content for seeing interpolated values everywhere
    epa.val <- paste0('<b>Interpolated ', f.titles.epa(input$EPA_var),
                      ': <b style = \"color:DodgerBlue\">', 
                      round(values(epa.ras), digits = 2),"</b></b>")
    
    ## Final content vector -------------
    epa.content <- paste0(site, id, city_state, lat_lon, avg.range)
    
    #Removes popups for which all data are NA
    #Coercing lat and lon to NA works better than removing these rows
    epa.content.df <- data.frame(cbind(lons, lats, 
                                       epa.content), stringsAsFactors = FALSE)
    epa.content.df[,1:2] <- sapply(epa.content.df[,1:2], as.numeric)
    #-----------
    
    #df of interpolated values to display
    epa.val.df <- data.frame(cbind(epa.lons, epa.lats, epa.val), 
                             stringsAsFactors = FALSE)
    epa.val.df[,1:2] <- sapply(epa.val.df[,1:2], as.numeric)
    epa.val.df[which(is.na(values(epa.ras))), 1:2] <- NA #Coerces lats and lons to NA where vals are NA
    
    pal.epa <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                            domain = vals,
                            na.color = "transparent"
                        )
    leg.pal.epa <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                domain = vals,
                                na.color = "transparent",
                                reverse = TRUE
                        )
    epa.mon.pal <- colorFactor(palette = "blue", domain = "EPA Monitor Locations")
    
    lon.center <- -75.15
    lat.center <- 40.00
    zoom.no <- 8
    button.js <- paste0("function(btn, map){ map.setView([", 
                        lat.center, ", ", lon.center, "], ", zoom.no, "); }")
    
                            
    leaflet(epa.content.df) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(epa.ras, colors = pal.epa, opacity = 0.8, method = "ngb") %>%
      addLegend(pal = leg.pal.epa, values = vals, opacity = 1,
                title = f.titles.epa(input$EPA_var), position = "topright",
                labFormat = myLabelFormat()) %>%
      addCircleMarkers(data = epa.val.df, lng = ~epa.lons, lat = ~epa.lats, popup = ~epa.val,
                       fillOpacity = 0.0001, stroke = FALSE) %>% #Add these first so they're lowest down
      addCircleMarkers(~lons, ~lats,
                       radius = 3, color = "blue", fillOpacity = 0.5, stroke = FALSE,
                       group = "Monitor Locations") %>%
      ###Add in transparent circle markers that are larger for easier clicking
      addCircleMarkers(~lons, ~lats, popup = ~epa.content,
                       radius = 5, color = "transparent", fillOpacity = 0.0001, stroke = FALSE,
                       group = "Monitor Locations") %>% 
      addMeasure(position = "topleft", primaryLengthUnit = "meters", secondaryLengthUnit = "miles",
                 primaryAreaUnit = "sqmeters", secondaryAreaUnit = "sqmiles") %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Recenter",
        onClick = JS(paste(button.js))
      )) %>%
      leafem::addMouseCoordinates() %>%
      addLegendEPA(colors = "blue", labels = "EPA Monitor Locations", sizes = 9) %>%
      addPolylines(data = county.borders, color = "black", weight = 1,
                   group = "County Borders") %>%
      addLayersControl(overlayGroups = c("Monitor Locations", "County Borders"),
                       options = layersControlOptions(collapsed = FALSE))
  })  
  
  observeEvent(input$EPA_go, {
    output$int.map.epa <- renderLeaflet({
      withProgress(message = "Loading map...", {epa.plot()})
    })
  })
  
  ###Data downloader
  output$download <- downloadHandler(
    filename = function() {
      paste0(strftime(Sys.time(), format = '%Y%m%d%H%M%S'), ".csv")
    },
    content = function(file) {
      withProgress(message = 'Preparing download...', 
        write.csv(download.data, file, row.names = FALSE)
      )
    }
  )
  
  output$EPA_download <- downloadHandler(
    filename = function() {
      startdate <- strftime(min(epa.df$Date), format = '%Y%m%d')
      enddate <- strftime(max(epa.df$Date), format = '%Y%m%d')
      paste0(epa.df.name, "_", startdate, "-", enddate, ".csv")
    },
    content = function(file) {
      withProgress(message = 'Preparing download...', 
                   write.csv(epa.df, file, row.names = FALSE)
      )
    }
  )
  
}#End server function