EPAMap <- function(rasBrick, popups = TRUE, bounds = NULL, colors = 'YlOrRd'){
  
  titles.df <- data.frame(
    variables = c('PM2.5', 'PM10', 'SO2', 'NO2', 'O3', 'CO'),
    leg.titles = c("Avg. PM\u2082.\u2085 Conc. (\u03BCg/m\u00B3)", 
                   "Avg. PM\u2081\u2080 Conc. (\u03BCg/m\u00B3)", 
                   "Avg. SO\u2082 Conc. (ppb)", "Avg. NO\u2082 Conc. (ppb)", 
                   "Avg. O\u2083 Conc. (ppb)", "Avg CO Conc. (ppm)"),
    html.titles = c('PM<sub>2.5</sub>', 'PM<sub>10</sub>', 'SO<sub>2</sub>',
                    'NO<sub>2</sub>', 'O<sub>3</sub>', 'CO'),
    units = c('\u03BCg/m\u00B3', '\u03BCg/m\u00B3', 'ppb', 'ppb', 'ppb', 'ppm'),
    subsc.layers = c('PM\u2082.\u2085', 'PM\u2081\u2080', 'SO\u2082', 'NO\u2082',
                     'O\u2083', 'CO')
  )

  colors <- brewer.pal(7, colors)
  
  lon.center <- (xmin(rasBrick) + xmax(rasBrick)) / 2
  lat.center <- (ymin(rasBrick) + ymax(rasBrick)) / 2
  zoom.no <- f.zoom(xmax(rasBrick) - xmin(rasBrick), ymax(rasBrick) - ymin(rasBrick))
  button.js <- paste0("function(btn, map){ map.setView([", 
                      lat.center, ", ", lon.center, "], ", zoom.no, "); }")
  
  layer.names <- titles.df$variables[which(titles.df$variables %in% names(rasBrick))]
  
  map <- leaflet(options = leafletOptions(crs = leafletCRS(
    proj4def = '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'))) %>%
    setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
    addProviderTiles(providers$Esri.WorldTopoMap) %>%
    addMeasure(position = "topleft", primaryLengthUnit = "meters", secondaryLengthUnit = "miles",
               primaryAreaUnit = "sqmeters", secondaryAreaUnit = "sqmiles") %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs", title = "Recenter",
      onClick = JS(paste(button.js))
    )) %>%
    leafem::addMouseCoordinates() %>%
    addLayersControl(overlayGroups = titles.df$subsc.layers,
                     options = layersControlOptions(collapsed = FALSE))
  
  if(popups){
    epa.lons <- xFromCell(rasBrick, 1:ncell(rasBrick))
    epa.lats <- yFromCell(rasBrick, 1:ncell(rasBrick))
  }
  
  for(i in 1:nlayers(rasBrick)){
    
    cur.layer <- rasBrick[[i]]
    
    name <- layer.names[i]
    
    vals <- values(cur.layer)
    
    mon.frame <- rasBrick@monitor_data[[i]]
    
    if(!all(is.na(vals))){
      assign('pal', 
             colorNumeric(palette = colors, 
                          domain = vals, 
                          na.color = "transparent"))
      assign('leg.pal', 
             colorNumeric(palette = colors, 
                          domain = vals, 
                          na.color = "transparent",
                          reverse = TRUE))
    } else{
      assign('pal',
             colorNumeric(palette = colors,
                          domain = 0,
                          na.color = "transparent"))
      assign('leg.pal', 
             colorNumeric(palette = colors, 
                          domain = 0, 
                          na.color = "transparent",
                          reverse = TRUE))
    }
    
    grp.name <- titles.df$subsc.layers[which(titles.df$variables == name)]
    leg.title <- titles.df$leg.titles[which(titles.df$variables == name)]
    html.title <- titles.df$html.titles[which(titles.df$variables == name)]
    unit <- titles.df$units[which(titles.df$variables == name)]
    
    map <- map %>%
      addRasterImage(cur.layer, colors = pal, 
                     opacity = 0.8, group = grp.name, method = "ngb") %>%
      addLegend(pal = leg.pal, values = vals, opacity = 1,
                title = leg.title, position = "topright",
                group = grp.name,
                labFormat = myLabelFormat())
    
    if(popups){
  
      mon.pal <- colorFactor(palette = "blue", domain = "EPA Monitor Locations")
      
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
      avg.range[grep('NA', avg.range)] <- paste0("Measurement duration not available")
      
      mon.lons <- mon.frame$Longitude
      mon.lats <- mon.frame$Latitude
      
      epa.content <- paste0(site, id, city_state, lat_lon, avg.range)
      
      epa.content.df <- data.frame(cbind(mon.lons, mon.lats, 
                                         epa.content), stringsAsFactors = FALSE)
      epa.content.df[,1:2] <- sapply(epa.content.df[,1:2], as.numeric)
      
      epa.val <- paste0('<b>Interpolated avg. ', html.title,
                        ' conc. (', unit, ') : <b style = \"color:DodgerBlue\">', 
                        round(vals, digits = 2),"</b></b>")
      
      epa.val.df <- data.frame(cbind(epa.lons, epa.lats, epa.val), 
                               stringsAsFactors = FALSE)
      epa.val.df[,1:2] <- sapply(epa.val.df[,1:2], as.numeric)
      epa.val.df[which(is.na(vals)), 1:2] <- NA
      
      map <- map %>%
        addCircleMarkers(data = epa.val.df,
                         ~epa.lons, ~epa.lats,
                         radius = 5, popup = ~epa.val,
                         fillOpacity = 0.0001, stroke = FALSE,
                         group = grp.name) %>%
        addCircleMarkers(data = epa.content.df,
                         ~mon.lons, ~mon.lats,
                         radius = 3, color = "blue", 
                         fillOpacity = 0.5, stroke = FALSE,
                         group = grp.name) %>%
        addCircleMarkers(data = epa.content.df,
                         ~mon.lons, ~mon.lats, popup = ~epa.content,
                         radius = 5, color = "transparent", 
                         fillOpacity = 0.0001, stroke = FALSE,
                         group = grp.name)
      
    }
    
  }
  
  if(!is.null(bounds)){
    
    if(!grepl('SpatialPolygons', class(bounds))){
      stop('bounds must be of class \"SpatialPolygons\" or \"SpatialPolygonsDataFrame\"')
    } else{
      
      map <- addPolylines(map, data = bounds, color = 'black', weight = 1)
      
    }
    
  }
  
  map <- map %>%
    hideGroup(titles.df$subsc.layers[-1]) %>%
    addLegendEPA(colors = "blue", labels = "EPA Monitor Locations", sizes = 9)
  
  return(map)
  
}