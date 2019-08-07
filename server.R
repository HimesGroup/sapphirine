
.libPaths("/home/maya/R/x86_64-pc-linux-gnu-library/3.4/") #mapview dependencies, use only for online version

source("global.R")

#Define server logic
server <- function(input, output, session){
  
  map.plot <- eventReactive(input$go, {
    
    #Creates base layer for raster layer to be added to map later
    r <- raster(nrows = input$row.no, ncols = input$col.no, xmn = input$lon.range[1], 
                xmx = input$lon.range[2], ymn = input$lat.range[1], ymx = input$lat.range[2])
    
    lons <- xFromCell(r, 1:ncell(r))
    lats <- yFromCell(r, 1:ncell(r))
    
    step.size.x <- (input$lon.range[2] - input$lon.range[1]) / input$col.no
    step.size.y <- (input$lat.range[2] - input$lat.range[1]) / input$row.no
    
    content <- vector()

    if(input$times[2] != "23:59"){
      upper.ind <- grep(input$times[2], mins) - 1
    }
    else{
      upper.ind <- grep("23:59", mins)
    }
    #Accounts for fact that time is subsetted by the hour  
    
    map.data <- app.data %>%
      subset(Day %in% input$dates[1]:input$dates[2]) %>%
      subset(Time %in% mins[grep(input$times[1], mins) : upper.ind])
    #Subsets data by user-selected date range and time-range
    #Removes rows containing NAs for selected measurement type
    
    sensor.data <- subset(map.data, Sensor.ID %in% c(input$sensors.hl, input$sensors.o))

    #Value map layers:
    
    for(i in 1:length(sensor.measures)){
      suffix <- f.suffix(sensor.measures[i])
      measure.data <- subset(sensor.data, !is.na(sensor.data[,sensor.measures[i]]))
      if(nrow(measure.data) > 0){
        assign(paste0("map.layer", suffix),
               rasterize(measure.data[,3:2], r, measure.data[,sensor.measures[i]], fun = mean, na.rm = TRUE),
               envir = .GlobalEnv)
      }
      else{
        assign(paste0("map.layer", suffix),
               rasterize(data.frame(NA, NA), r, na.rm = TRUE),
               envir = .GlobalEnv)
      }
    }
    
    crime.data <- subset(app.data, !is.na(app.data$Crime))
    assign("map.layer.c", rasterize(crime.data[,3:2], r, crime.data$Crime, fun = sum, na.rm = TRUE), 
           envir = .GlobalEnv)
    
    assign("map.layer.pov", resample(pov.raster, r, method = "bilinear"), envir = .GlobalEnv)
    
    assign("map.layer.tr", resample(traffic.raster, r, method = "bilinear"), envir = .GlobalEnv)
      
    for(i in 1:length(sensor.measures)){
      suffix <- f.suffix(sensor.measures[i])
      measure.data <- subset(sensor.data, !is.na(sensor.data[,sensor.measures[i]]))
      if(nrow(measure.data) > 0){
        assign("density.raster", 
               rasterize(measure.data[,3:2], r, measure.data$Count, fun = sum, na.rm = TRUE))
      }
      else{
        assign("density.raster",
               rasterize(data.frame(NA, NA), r, na.rm = TRUE))
      }       
      assign(paste0("map.layer", suffix, ".d"), density.raster, envir = .GlobalEnv)
      assign(paste0("map.layer", suffix, ".dlog"), 
             calc(density.raster, fun = function(x){log10(x)}), envir = .GlobalEnv)
    }
    
    for(i in 1:length(values(map.layer.pm2.5))){
      content[i] <- paste0("<b>",
        "Lat rng: [", "<b style = \"color:DimGray\">", format(round(lats[i] - step.size.y/2, 5), nsmall = 5), "</b>", ", ", 
        "<b style = \"color:DimGray\">", format(round(lats[i] + step.size.y/2, 5), nsmall = 5), "</b>", "]", "<br/>",
        "Lon rng: [", "<b style = \"color:DimGray\">", format(round(lons[i] - step.size.x/2, 5), nsmall = 5), "</b>", ", ", 
        "<b style = \"color:DimGray\">", format(round(lons[i] + step.size.x/2, 5), nsmall = 5), "</b>", "]", "<br/>",
        "Avg. temperature: ", 
        if(!is.na(map.layer.t[i])){
          paste0("<b style = \"color:DodgerBlue\">", round(values(map.layer.t)[i], digits = 1), "\u00B0C", "</b>",
                 " (", values(map.layer.t.d)[i], ")")}
        else{paste0("<b style = \"color:Tomato\">", "no data", "</b>", " (0)")}, "<br/>",
        "Avg. humidity: ", 
        if(!is.na(map.layer.h[i])){
          paste0("<b style = \"color:DodgerBlue\">", round(values(map.layer.h)[i], digits = 1), "%", "</b>",
                 " (", values(map.layer.h.d)[i], ")")}
        else{paste0("<b style = \"color:Tomato\">", "no data", "</b>", " (0)")}, "<br/>",
        "Avg. PM1: ", 
        if(!is.na(map.layer.pm1[i])){
          paste0("<b style = \"color:DodgerBlue\">", round(values(map.layer.pm1)[i], digits = 2), " \u03BCg/m\u00B3", "</b>",
                 " (", values(map.layer.pm1.d)[i], ")")}
        else{paste0("<b style = \"color:Tomato\">", "no data", "</b>", " (0)")}, "<br/>",
        "Avg. PM2.5: ", 
        if(!is.na(map.layer.pm2.5[i])){
          paste0("<b style = \"color:DodgerBlue\">", round(values(map.layer.pm2.5)[i], digits = 2), " \u03BCg/m\u00B3", "</b>",
                 " (", values(map.layer.pm2.5.d)[i], ")")}
        else{paste0("<b style = \"color:Tomato\">", "no data", "</b>", " (0)")}, "<br/>",
        "Avg. PM10: ", 
        if(!is.na(map.layer.pm10[i])){
          paste0("<b style = \"color:DodgerBlue\">", round(values(map.layer.pm10)[i], digits = 2), " \u03BCg/m\u00B3", "</b>",
                 " (", values(map.layer.pm10.d)[i], ")")}
        else{paste0("<b style = \"color:Tomato\">", "no data", "</b>", " (0)")}, "<br/>",
        "# reported crimes: ",
        if(point.in.polygon(xFromCell(map.layer.c, i), yFromCell(map.layer.c, i), 
                            city.border$Longitude, city.border$Latitude) == 1){
          if(!is.na(map.layer.c[i])){
            paste0("<b style = \"color:DodgerBlue\">", values(map.layer.c)[i], "</b>")
          }
          else{
            paste0("<b style = \"color:DodgerBlue\">", "0", "</b>")
          }
        }
        else{
          if(!is.na(map.layer.c[i])){
            paste0("<b style = \"color:DodgerBlue\">", values(map.layer.c)[i], "</b>", 
                   " (", "<b style = \"color:Tomato\">", "Phil. only", "</b>", ")")
          }
          else{
            paste0("<b style = \"color:Tomato\">", "no data", "</b>")
          }
        },
           "<br/>",
        "Avg. ADI: ",
        if(!is.na(map.layer.pov[i])){paste0("<b style = \"color:DodgerBlue\">", round(values(map.layer.pov)[i], digits = 2), "</b>")}
        else{paste0("<b style = \"color:Tomato\">", "no data", "</b>")}, "<br/>",
        "Avg. AADT: ",
        if(is.na(map.layer.tr[i])){
          paste0("<b style = \"color:Tomato\">", "no data", "</b>")
        }
        else{
          if(point.in.polygon(xFromCell(map.layer.tr, i), yFromCell(map.layer.tr, i), 
                              city.border$Longitude, city.border$Latitude) == 1){
            paste0("<b style = \"color:DodgerBlue\">", round(values(map.layer.tr)[i], digits = 0), "</b>")
          }
          else{
            paste0("<b style = \"color:DodgerBlue\">", round(values(map.layer.tr)[i], digits = 0), "</b>", 
                   " (", "<b style = \"color:Tomato\">", "Phil. only", "</b>", ")")
          }
        },
        "<br/>",
        "</b>"
      )
    }
    
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
    #Indicies for removing popups with all NA
    
    content.df <- data.frame(cbind(lons, lats, content), stringsAsFactors = FALSE)
    content.df[,1:2] <- sapply(content.df[,1:2], as.numeric)
    content.df[inds, 1:2] <- NA
    #Removes popups for which all data are NA
    #Coercing lat and lon to NA works better than removing these rows

    colors <- c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c", "#b10026")
    
    for(i in 1:length(all.measures)){
      suffix <- f.suffix(all.measures[i])
      vals <- values(eval(parse(text = paste0("map.layer", suffix))))
      if(!all(is.na(vals))){
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
        assign(paste0("pal", suffix, ".d"),
               colorNumeric(palette = colors, 
                            domain = vals, 
                            na.color = "transparent"),
               envir = .GlobalEnv)
        assign(paste0("leg.pal", suffix, ".d"), 
               colorNumeric(palette = colors, 
                            domain = vals, 
                            na.color = "transparent",
                            reverse = TRUE),
               envir = .GlobalEnv)
      }
      else{
        assign(paste0("pal", suffix, ".d"),
               colorNumeric(palette = colors,
                            domain = 0,
                            na.color = "transparent"),
               envir = .GlobalEnv)
        assign(paste0("leg.pal", suffix, ".d"), 
               colorNumeric(palette = colors, 
                            domain = 0, 
                            na.color = "transparent",
                            reverse = TRUE),
               envir = .GlobalEnv)
      }
    }
    
    
    leaflet(content.df) %>%
      setView(lng = -75.40, lat = 39.70, zoom = 8) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(map.layer.pm2.5, colors = pal.pm2.5, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
      addLegend(pal = leg.pal.pm2.5, values = values(map.layer.pm2.5), opacity = 1,
                title = toString(f.titles("PM2.5")), position = "topright",
                group = "Measurement value") %>%
      addRasterImage(map.layer.pm2.5.dlog, colors = pal.pm2.5.d, opacity = 0.8, group = "Measurement density", method = "ngb") %>%
      addLegend(pal = leg.pal.pm2.5.d, values = values(map.layer.pm2.5.dlog), opacity = 1, 
                title = paste("log # of PM2.5 data points"),
                group = "Measurement density", position = "topright") %>%
      addCircleMarkers(~lons, ~lats, popup = ~content, stroke = FALSE, fillOpacity = 0.001) %>%
      addMeasure(position = "topleft", primaryLengthUnit = "meters", secondaryLengthUnit = "miles",
                 primaryAreaUnit = "sqmeters", secondaryAreaUnit = "sqmiles") %>%
      addEasyButton(easyButton(
        icon = "fa-crosshairs", title = "Recenter",
        onClick = JS("function(btn, map){ map.setView([39.70, -75.40], 8); }")
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
  
  
  observeEvent(input$int.map_groups, {
    
    map <- leafletProxy("int.map", session) %>%
      clearControls() %>%
      removeMarker(c("null1", "null2", "null3")) %>%
      clearPopups()

    if("Measurement value" %in% input$int.map_groups && !"Measurement density" %in% input$int.map_groups){
      
      for(i in c(4, 1:3, 5:length(all.measures))){ #Ensures that default is PM2.5, the 4th item
        
        suffix <- f.suffix(all.measures[i])
        map.layer <- eval(parse(text = paste0("map.layer", suffix)))
        legend.title <- toString(f.titles(all.measures[i]))
        
        if(all.measures[i] %in% input$int.map_groups){
          
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
            
            map %>%
              clearImages() %>%
              removeMarker("null1") %>%
              addRasterImage(map.layer, colors = pal, opacity = 0.8, method = "ngb") %>%
              addLegend(pal = leg.pal, values = values(map.layer), opacity = 1,
                        title = legend.title, position = "topright") %>%
              showGroup(c(all.measures[i], "Measurement value")) %>%
              hideGroup(c(all.measures[which(all.measures != all.measures[i])], "Measurement density"))
          }
        }
      }
    }
    
    else if("Measurement density" %in% input$int.map_groups && !"Measurement value" %in% input$int.map_groups){
      
      for(i in c(4, 1:3, 5:length(sensor.measures))){
        
        suffix <- f.suffix(sensor.measures[i])
        map.layer <- eval(parse(text = paste0("map.layer", suffix, ".dlog")))
        legend.title <- toString(f.titles.d(sensor.measures[i]))
        
        if(sensor.measures[i] %in% input$int.map_groups){
          
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
            
            map %>%
              clearImages() %>%
              removeMarker("null1") %>%
              addRasterImage(map.layer, colors = pal, opacity = 0.8, method = "ngb") %>%
              addLegend(pal = leg.pal, values = values(map.layer), opacity = 1,
                        title = legend.title, position = "topright") %>%
              showGroup(c(sensor.measures[i], "Measurement density")) %>%
              hideGroup(c(all.measures[which(all.measures != all.measures[i])], "Measurement value"))
          }
        }
      }
      
      if("Crime" %in% input$int.map_groups || "Poverty" %in% input$int.map_groups || 
         "Traffic" %in% input$int.map_groups){
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