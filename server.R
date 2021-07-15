
source("global.R")

#Define server logic
server <- function(input, output, session){
  
  observeEvent(input$Instructions_b1, {
    toggle("Instructions_1")
  })
  
  #observe({click('go')})

  ## Plot map on GO -------------
  map.plot <- eventReactive(input$go, {
    
    #Creates base layer for raster layer to be added to map later
     
    r.shape <- switch(input$geoBounds,
                      'Rectangle' = extent(input$lon.range[1], input$lon.range[2], 
                                           input$lat.range[1], input$lat.range[2]) %>%
                        as('SpatialPolygons'),
                      'Counties' = selectGPACounties(input$cty.bounds)[,'NAME'],
                      'ZIP Codes' = zipcodes[which(zipcodes$ZCTA5CE10 %in%
                                                     unlist(strsplit(input$zip.bounds, split = ', '))),'ZCTA5CE10']
    )
    crs(r.shape) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    if(input$geoBounds != 'Rectangle'){
      r.shape <- gUnaryUnion(r.shape)
    }
    assign("r.shape", r.shape, envir = .GlobalEnv)
    
    binType <- switch(input$binType,
                      'Rectangles' = 'Rectangles',
                      'ZIP Codes' = zipcodes[,'ZCTA5CE10'],
                      'Block Groups' = blockGs[,'AFFGEOID'],
                      'Counties' = GPA_counties[,'NAME'])
    
    assign('display.ctys', raster::intersect(GPA_counties, r.shape) %>% geometry(),
           envir = .GlobalEnv)
    assign('display.zips', raster::intersect(zipcodes, r.shape) %>% geometry(),
           envir = .GlobalEnv)
    assign('display.blockGs', raster::intersect(blockGs, r.shape) %>% geometry(),
           envir = .GlobalEnv)
    
    if(input$binType == 'Rectangles'){
      
      r <- raster(nrows = input$row.no, ncols = input$col.no, xmn = xmin(r.shape), 
                  xmx = xmax(r.shape), ymn = ymin(r.shape), ymx = ymax(r.shape))
      crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      
      lons <- xFromCell(r, 1:ncell(r))
      lats <- yFromCell(r, 1:ncell(r))
      
      step.size.x <- (input$lon.range[2] - input$lon.range[1]) / input$col.no
      step.size.y <- (input$lat.range[2] - input$lat.range[1]) / input$row.no
      
    } else {
      
      r <- binType %>% raster::intersect(r.shape)
      r <- r[,1]
      names(r)[1] <- gsub('\\.1', '', names(r)[1])
      
      if(length(r) > 0){
        row.names(r) <- make.names(row.names(r), unique = TRUE)
      }
      
      latlon <- f.centroids(r)
      
      assign('lons', latlon$lons, envir = .GlobalEnv)
      assign('lats', latlon$lats, envir = .GlobalEnv)
      
      assign('r', r, envir = .GlobalEnv)
      
    }
    
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
          mins[grep(input$times[1], mins) : upper.ind]
        #, point.in.SpatialPolygons(Longitude, Latitude, r.shape)
        )
    
    sensor.data <- map.data %>% dplyr::filter(Sensor.ID %in% input$sensors)
    
    assign('download.data', subset(sensor.data, 
                                   select = -Count), 
           envir = .GlobalEnv)
    
    # Remove big objects and clear memory -------------
    crime.data <- map.data %>% dplyr::filter(!is.na(Crime)) 
    
    rm(map.data)

    ## Value map layers: -------------
    
    # Crime raster
    
    if(input$binType == 'Rectangles'){
      assign("map.layer.c", 
             rasterize(crime.data[,3:2], r, crime.data$Crime, fun = sum, na.rm = TRUE) %>%
               crop(extent(r.shape)) %>% mask(r.shape), 
             envir = .GlobalEnv)
    } else {
      crime.sp <- SpatialPointsDataFrame(crime.data[,3:2], crime.data[,'Crime'],
                      proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')) %>%
        crop(r.shape)
      map.layer.c <- try(cbind(r, over(r, crime.sp[,'Crime'], fn = 'sum', na.rm = TRUE)),
                         silent = TRUE)
      if(is(map.layer.c, 'try-error')){
        map.layer.c <- cbind(binType, data.frame('Crime' = NA))
      }
      assign("map.layer.c", map.layer.c, envir = .GlobalEnv)
    }
    rm(crime.data)
    
    # ADI raster
    if(input$binType == 'Rectangles'){
      assign("map.layer.pov", 
             rasterize(pov.shp, r, field = pov.shp$ADI_NAT, fun = mean, na.rm = TRUE) %>%
               crop(extent(r.shape)) %>% mask(r.shape),
             envir = .GlobalEnv)
    } else {
      map.layer.pov <- try(cbind(r, over(r, pov.shp[,'ADI_NAT'], fn = 'mean', na.rm = TRUE)),
                           silent = TRUE)
      if(is(map.layer.pov, 'try-error')){
        map.layer.pov <- cbind(binType, data.frame('ADI_NAT' = NA))
      }
      assign('map.layer.pov', map.layer.pov, envir = .GlobalEnv)
    }
    
    # Traffic raster
    if(input$binType == 'Rectangles'){
      assign("map.layer.tr", 
             try(resample(traffic.raster, r, method = "bilinear") %>%
                   crop(extent(r.shape)) %>% mask(r.shape), silent = TRUE),
             envir = .GlobalEnv)
      if(is(map.layer.tr, 'try-error')){
        assign("map.layer.tr", 
               rasterize(data.frame(NA, NA), r, na.rm = TRUE) %>%
                 crop(extent(r.shape)) %>% mask(r.shape), envir = .GlobalEnv)
      }
    } else {
       traffic.sp <- as(traffic.raster, 'SpatialPointsDataFrame')
       map.layer.tr <- try(cbind(r, over(r, traffic.sp, fn = 'mean', na.rm = TRUE)),
                           silent = TRUE)
       if(is(map.layer.tr, 'try-error')){
         map.layer.tr <- cbind(binType, data.frame('Traffic' = NA))
       } else {
        map.layer.tr@data <- rename(map.layer.tr@data, 'Traffic' = layer)
       }
       assign('map.layer.tr', map.layer.tr, envir = .GlobalEnv)
    }
    
    # Tree cover raster
    if(input$binType == 'Rectangles'){
      assign("map.layer.tc", 
             try(resample(treeCover, r, method = "bilinear") %>%
                   crop(extent(r.shape)) %>% mask(r.shape), silent = TRUE),
             envir = .GlobalEnv)
      if(is(map.layer.tc, 'try-error')){
        assign("map.layer.tc", 
               rasterize(data.frame(NA, NA), r, na.rm = TRUE) %>%
                 crop(extent(r.shape)) %>% mask(r.shape), envir = .GlobalEnv)
      }
    } else {
      tree.sp <- as(treeCover, 'SpatialPointsDataFrame')
      map.layer.tc <- try(cbind(r, over(r, tree.sp, fn = 'mean', na.rm = TRUE)),
                          silent = TRUE)
      if(is(map.layer.tc, 'try-error')){
        map.layer.tc <- cbind(binType, data.frame('TreeCover' = NA))
      } else {
        map.layer.tc@data <- rename(map.layer.tc@data, 'TreeCover' = layer)
      }
      assign('map.layer.tc', map.layer.tc, envir = .GlobalEnv)
    }
    
    # Value and density sensor rasters
    for(i in 1:length(sensor.measures)){
      suffix <- f.suffix(sensor.measures[i])
      measure.data <- dplyr::filter(sensor.data, !is.na(eval(parse(text = sensor.measures[i]))))
      
      if(input$binType == 'Rectangles'){
      
        if(nrow(measure.data) > 0){
          assign("density.raster", 
                 rasterize(measure.data[,3:2], r, measure.data$Count, fun = sum, na.rm = TRUE) %>%
                   crop(extent(r.shape)) %>% mask(r.shape))
          assign(paste0("map.layer", suffix),
                 rasterize(measure.data[,3:2], r, measure.data[,sensor.measures[i]], fun = mean, na.rm = TRUE) %>%
                   crop(extent(r.shape)) %>% mask(r.shape),
                 envir = .GlobalEnv)
        } else {
          assign("density.raster",
                 rasterize(data.frame(NA, NA), r, na.rm = TRUE))
          assign(paste0("map.layer", suffix),
                 rasterize(data.frame(NA, NA), r, na.rm = TRUE),
                 envir = .GlobalEnv)
        }       

      } else {
        
        if(nrow(measure.data) > 0){
          sp.frame <- SpatialPointsDataFrame(measure.data[,3:2], measure.data[,c(sensor.measures[i], 'Count')],
                        proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
          assign("density.raster", 
                 try(cbind(r, over(r, sp.frame[,'Count'], fn = 'sum', na.rm = TRUE)),
                     silent = TRUE))
          if(is(density.raster, 'try-error')){
            density.raster <- cbind(binType, data.frame('Count' = NA))
          }
          map.layer <- try(cbind(r, over(r, sp.frame[,sensor.measures[i]], fn = 'mean', na.rm = TRUE)),
                           silent = TRUE)
          if(is(map.layer, 'try-error')){
            map.layer <- cbind(binType, tibble(!!paste(sensor.measures[i]) := NA))
          }
          assign(paste0("map.layer", suffix),
                 map.layer,
                 envir = .GlobalEnv)
        } else {
          sp.frame <- SpatialPointsDataFrame(data.frame(lons,lats), #Will give NA frame with right row number
                                             tibble(!!sensor.measures[i] := rep(NA, length(lats)), 
                                                        Count = rep(NA, length(lats))),
                        proj4string = CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'))
          assign("density.raster",
                 try(cbind(r, over(r, sp.frame[,'Count']))),
                     silent = TRUE)
          if(is(density.raster, 'try-error')){
            density.raster <- cbind(binType, data.frame('Count' = NA))
          }
          map.layer <- try(cbind(r, over(r, sp.frame[,sensor.measures[i]])),
                           silent = TRUE)
          if(is(map.layer, 'try-error')){
            map.layer <- cbind(binType, tibble(!!paste(sensor.measures[i]) := NA))
          }
          assign(paste0("map.layer", suffix),
                 map.layer,
                 envir = .GlobalEnv)
        }   
        
      }
      
      assign(paste0("map.layer", suffix, ".d"), density.raster, envir = .GlobalEnv)
      
      if(input$binType == 'Rectangles'){
        assign(paste0("map.layer", suffix, ".dlog"), 
               calc(density.raster, fun = function(x){log10(x)}), envir = .GlobalEnv)
      } else {
        dlog.layer <- density.raster
        dlog.layer@data[,'Count'] <- log10(dlog.layer@data[,'Count'])
        assign(paste0("map.layer", suffix, '.dlog'), dlog.layer, envir = .GlobalEnv)
      }
      
    }
    
    ## Content format
    if(input$binType == 'Rectangles'){ -----------
      
      assign('total_length', seq(1, length(values(map.layer.pm2.5))), envir = .GlobalEnv)
      lat_lon <- vector()
      lat_lon <- paste0("<b>",
                        "Lat rng: [", "<b style = \"color:DimGray\">",gsub(" ", "", format(round(lats[total_length] - step.size.y/2, 5), nsmall = 5), fixed = TRUE), "</b>", ", ",
                        "<b style = \"color:DimGray\">",gsub(" ", "", format(round(lats[total_length] + step.size.y/2, 5), nsmall = 5), fixed = TRUE), "</b>", "]", "<br/>",
                        "Lon rng: [", "<b style = \"color:DimGray\">",gsub(" ", "", format(round(lons[total_length] - step.size.x/2, 5), nsmall = 5), fixed = TRUE), "</b>", ", ",
                        "<b style = \"color:DimGray\">",gsub(" ", "", format(round(lons[total_length] + step.size.x/2, 5), nsmall = 5), fixed = TRUE), "</b>", "]", "<br/>") 
      
      #Descriptor
      sensl <- '<br/><b style = \"color:#660066\">As measured by sensors:</b><br/>'
      
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
      indx1 <- intersect(which(!is.na(values(map.layer.c))),which(cpoints))
      crimel[indx1] <- paste0("# reported crimes: ","<b style = \"color:DodgerBlue\">", values(map.layer.c)[indx1], "</b>","<br/>")
      
      # NA with cpoint = 1
      indx2 <- intersect(which(is.na(values(map.layer.c))),which(cpoints))
      crimel[indx2] <- paste0("# reported crimes: ","<b style = \"color:DodgerBlue\">", "0", "</b>","<br/>")
      
      # not NA with cpoint != 1
      indx3 <- intersect(which(!is.na(values(map.layer.c))),which(!cpoints))
      crimel[indx3] <- paste0("# reported crimes: ","<b style = \"color:DodgerBlue\">", values(map.layer.c)[indx3], "</b>",
                              " (", "<b style = \"color:Tomato\">", "Phil. only", "</b>", ")","<br/>")
      
      # NA with cpoint != 1
      indx4 <- intersect(which(is.na(values(map.layer.c))),which(!cpoints))
      crimel[indx4] <- paste0("# reported crimes: ","<b style = \"color:Tomato\">", "no data", "</b>","<br/>")
      
      # Poverty
      povl <- vector()
      povl[which(!is.na(values(map.layer.pov)))] <- paste0("Avg. ADI: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.pov)[which(!is.na(values(map.layer.pov)))], digits = 2), "</b>","<br/>")
      povl[which(is.na(values(map.layer.pov)))] <- paste0("Avg. ADI: ","<b style = \"color:Tomato\">", "no data", "</b>", "<br/>")
      
      # Traffic
      trafl <- vector()
      trafl[which(!is.na(values(map.layer.tr)))] <- paste0("Avg. AADT: ","<b style = \"color:DodgerBlue\">", round(values(map.layer.tr)[which(!is.na(values(map.layer.tr)))], digits = 0), "</b>","<br/>")
      trafl[which(is.na(values(map.layer.tr)))] <- paste0("Avg. AADT: ","<b style = \"color:Tomato\">", "no data", "</b>","<br/>")
    
      # Tree Cover
      treel <- vector()
      treel[which(!is.na(values(map.layer.tc)))] <- paste0("Tree Cover: ", "<b style = \"color:DodgerBlue\">", round(values(map.layer.tc)[which(!is.na(values(map.layer.tc)))], digits = 1), "</b>", "%", "<br/>", "</b>")
      treel[which(is.na(values(map.layer.tc)))] <- paste0("Tree Cover: ","<b style = \"color:Tomato\">", "0", "</b>","%","<br/>", "</b>")
      
      
      ## Final content vector -------------
      content <- paste0(lat_lon,sensl,templ,humidl,pm1l,pm2.5l,pm10l,'<br/>',crimel,povl,trafl,treel)
      
      #Indicies for removing popups with all NA
      inds.df <- cbind(values(map.layer.t),
                       values(map.layer.h),
                       values(map.layer.pm1),
                       values(map.layer.pm2.5),
                       values(map.layer.pm10),
                       values(map.layer.c),
                       values(map.layer.pov),
                       values(map.layer.tr),
                       values(map.layer.tc)
      )
      
      inds <- apply(inds.df, 1, function(x) all(is.na(x)))
      
      #Removes popups for which all data are NA
      #Coercing lat and lon to NA works better than removing these rows
      content.df <- data.frame(cbind(lons, lats, content), stringsAsFactors = FALSE)
      content.df[,1:2] <- sapply(content.df[,1:2], as.numeric)
      content.df[inds, 1:2] <- NA
      
    } else {-----------
        
        assign('total_length', seq(1,length(r)), envir = .GlobalEnv)
        
        loc <- vector()
        
        if(input$binType == 'Counties'){
          loc <- paste0("<b>",
                        r$NAME, " County", "<br/>")
        
        } else if(input$binType == 'ZIP Codes'){
          loc <- paste0("<b>",
                        "ZIP Code: ", r$ZCTA5CE10, "<br/>")
          
          
        } else if(input$binType == 'Block Groups'){
          loc <- paste0("<b>",
                        "GeoID: ", "<b style = \"color:DimGray\">", gsub('1500000US', '', r$AFFGEOID), "</b>", "<br/>")
        }
        
        #Descriptor
        sensl <- '<br/><b style = \"color:#660066\">As measured by sensors:</b><br/>'
        
        # Temperature
        templ <- vector()
        templ[which(!is.na(map.layer.t$Temperature))] <- paste0("Avg. temperature: ","<b style = \"color:DodgerBlue\">", round(map.layer.t$Temperature[which(!is.na(map.layer.t$Temperature))], digits = 1),"\u00B0C", "</b>"," (", map.layer.t.d$Count[which(!is.na(map.layer.t$Temperature))], ")","<br/>")
        templ[which(is.na(map.layer.t$Temperature))] <- paste0("Avg. temperature: ","<b style = \"color:Tomato\">", "no data", "</b>", " (0)", "<br/>")
        
        
        # Humidity
        humidl <- vector()
        humidl[which(!is.na(map.layer.h$Humidity))] <- paste0("Avg. humidity: ","<b style = \"color:DodgerBlue\">", round(map.layer.h$Humidity[which(!is.na(map.layer.h$Humidity))], digits = 1),"%", "</b>"," (", map.layer.h.d$Count[which(!is.na(map.layer.h$Humidity))], ")","<br/>")
        humidl[which(is.na(map.layer.h$Humidity))] <- paste0("Avg. humidity: ","<b style = \"color:Tomato\">", "no data", "</b>", " (0)", "<br/>")
        
        # PM1
        pm1l <- vector()
        pm1l[which(!is.na(map.layer.pm1$PM1))] <- paste0("Avg. PM<sub>1</sub>: ","<b style = \"color:DodgerBlue\">", round(map.layer.pm1$PM1[which(!is.na(map.layer.pm1$PM1))], digits = 2)," \u03BCg/m\u00B3", "</b>"," (", map.layer.pm1.d$Count[which(!is.na(map.layer.pm1$PM1))], ")","<br/>")
        pm1l[which(is.na(map.layer.pm1$PM1))] <- paste0("Avg. PM<sub>1</sub>: ","<b style = \"color:Tomato\">", "no data", "</b>", " (0)", "<br/>")
        
        # PM2.5l
        pm2.5l <- vector()
        pm2.5l[which(!is.na(map.layer.pm2.5$PM2.5))] <- paste0("Avg. PM<sub>2.5</sub>: ","<b style = \"color:DodgerBlue\">", round(map.layer.pm2.5$PM2.5[which(!is.na(map.layer.pm2.5$PM2.5))], digits = 2)," \u03BCg/m\u00B3", "</b>"," (", map.layer.pm2.5.d$Count[which(!is.na(map.layer.pm2.5$PM2.5))], ")","<br/>")
        pm2.5l[which(is.na(map.layer.pm2.5$PM2.5))] <- paste0("Avg. PM<sub>2.5</sub>: ","<b style = \"color:Tomato\">", "no data", "</b>", " (0)", "<br/>")
        
        # PM10l
        pm10l <- vector()
        pm10l[which(!is.na(map.layer.pm10$PM10))] <- paste0("Avg. PM<sub>10</sub>: ","<b style = \"color:DodgerBlue\">", round(map.layer.pm10$PM10[which(!is.na(map.layer.pm10$PM10))], digits = 2)," \u03BCg/m\u00B3", "</b>"," (", map.layer.pm10.d$Count[which(!is.na(map.layer.pm10$PM10))], ")","<br/>")
        pm10l[which(is.na(map.layer.pm10$PM10))] <- paste0("Avg. PM<sub>10</sub>: ","<b style = \"color:Tomato\">", "no data", "</b>", " (0)", "<br/>")
        
        # Crime
        crimel <- vector()
        cpoints <- try(which(r@data[,1] %in% (raster::intersect(r, city.border)[,1])@data[,1]),
                        silent = TRUE)
        if(is(cpoints, 'try-error')){
          cpoints <- NA
        }
        
        # not NA with cpoint = 1
        indx1 <- intersect(which(!is.na(map.layer.c$Crime)), cpoints)
        crimel[indx1] <- paste0("# reported crimes: ","<b style = \"color:DodgerBlue\">", map.layer.c$Crime[indx1], "</b>","<br/>")
        
        # NA with cpoint = 1
        indx2 <- intersect(which(is.na(map.layer.c$Crime)), cpoints)
        crimel[indx2] <- paste0("# reported crimes: ","<b style = \"color:DodgerBlue\">", "0", "</b>",
                                " (", "<b style = \"color:Tomato\">", "Phil. only", "</b>", ")","<br/>")
        
        # not NA with cpoint != 1
        indx3 <- setdiff(which(!is.na(map.layer.c$Crime)), cpoints)
        crimel[indx3] <- paste0("# reported crimes: ","<b style = \"color:DodgerBlue\">", map.layer.c$Crime[indx3], "</b>",
                                " (", "<b style = \"color:Tomato\">", "Phil. only", "</b>", ")","<br/>")
        
        # NA with cpoint != 1
        indx4 <- setdiff(which(is.na(map.layer.c$Crime)), cpoints)
        crimel[indx4] <- paste0("# reported crimes: ","<b style = \"color:Tomato\">", "no data", "</b>",
                                " (", "<b style = \"color:Tomato\">", "Phil. only", "</b>", ")","<br/>")
        
        # Poverty
        povl <- vector()
        povl[which(!is.na(map.layer.pov$ADI_NAT))] <- paste0("Avg. ADI: ","<b style = \"color:DodgerBlue\">", round(map.layer.pov$ADI_NAT[which(!is.na(map.layer.pov$ADI_NAT))], digits = 2), "</b>","<br/>")
        povl[which(is.na(map.layer.pov$ADI_NAT))] <- paste0("Avg. ADI: ","<b style = \"color:Tomato\">", "no data", "</b>", "<br/>")
        
        # Traffic
        trafl <- vector()
        trpoints <- try(which(r@data[,1] %in% r@data[point.in.SpatialPolygons(lons, lats, PA),1]),
                        silent = TRUE)
        if(is(trpoints, 'try-error')){
          trpoints <- NA
        }
        ###Centroids are best way to account for partial intersections
        ###Some extras are still listed in eras, but none with measurement values in them
        ###Works because it still won't include counties that aren't selected (it'll just errantly report some selected ones as being inside PA/Philadelphia)

        #Not NA, fully in PA
        indx1 <- intersect(which(!is.na(map.layer.tr$Traffic)), trpoints)
        trafl[indx1] <- paste0("Avg. AADT: ","<b style = \"color:DodgerBlue\">", round(map.layer.tr$Traffic[indx1], digits = 0), "</b>","<br/>")
        
        #Not NA, outside PA (polygon is marginally overlapping)
        indx2 <- setdiff(which(!is.na(map.layer.tr$Traffic)), trpoints)
        trafl[indx2] <- paste0("Avg. AADT: ","<b style = \"color:DodgerBlue\">", round(map.layer.tr$Traffic[indx2], digits = 0), "</b>",
                               " (", "<b style = \"color:Tomato\">", "PA only", "</b>", ")", "<br/>")
        
        #NA
        trafl[which(is.na(map.layer.tr$Traffic))] <- paste0("Avg. AADT: ","<b style = \"color:Tomato\">", "no data", "</b>","<br/>")
      
        # Tree Cover
        treel <- vector()
        treel[which(!is.na(map.layer.tc$TreeCover))] <- paste0("% Tree Cover: ","<b style = \"color:DodgerBlue\">", round(map.layer.tc$TreeCover[which(!is.na(map.layer.tc$TreeCover))], digits = 2), "</b>","<br/>", "</b>")
        treel[which(is.na(map.layer.tc$TreeCover))] <- paste0("% Tree Cover: ","<b style = \"color:Tomato\">", "0", "</b>", "<br/>", "</b>")
        
        
        ## Final content vector -------------
        sp.content <- paste0(loc,sensl,templ,humidl,pm1l,pm2.5l,pm10l,'<br/>',crimel,povl,trafl,treel)
        assign('sp.content', sp.content, envir = .GlobalEnv)
    }
    
    #Remove big objects -------------
    rm(lat_lon,loc,templ,humidl,pm1l,pm2.5l,pm10l,crimel,povl,trafl,treel)
    rm(inds.df)

    colors <- brewer.pal(7, "YlOrRd")
    colors.d <- brewer.pal(7, "Purples")
    
    for(i in 1:length(all.measures)){
      suffix <- f.suffix(all.measures[i])
      if(input$binType == 'Rectangles'){
        vals <- values(eval(parse(text = paste0("map.layer", suffix))))
      } else {
        vname <- all.measures[i]
        if(vname == 'Area Deprivation Index'){vname <- 'ADI_NAT'}
        if(vname == 'Tree Cover'){vname <- 'TreeCover'}
        vals <- eval(parse(text = paste0('map.layer', suffix, '$', vname)))
      }
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
      if(input$binType == 'Rectangles'){
        vals <- values(eval(parse(text = paste0("map.layer", suffix, ".dlog"))))
      } else {
        vals <- eval(parse(text = paste0('map.layer', suffix, '.dlog$Count')))
      }
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
    
    assign('lon.center', (xmin(r.shape) + xmax(r.shape)) / 2,
           envir = .GlobalEnv)
    assign('lat.center', (ymin(r.shape) + ymax(r.shape)) / 2,
           envir = .GlobalEnv)
    assign('zoom.no', f.zoom(xmax(r.shape) - xmin(r.shape), ymax(r.shape) - ymin(r.shape)),
           envir = .GlobalEnv)
    
    button.js <- paste0("function(btn, map){ map.setView([", lat.center, ", ", lon.center, "], ", zoom.no, "); }")
    
    if(input$binType == 'Rectangles'){
      vals <- values(map.layer.pm2.5)
      vals.d <- values(map.layer.pm2.5.dlog)
    } else {
      vals <- map.layer.pm2.5$PM2.5
      vals.d <- map.layer.pm2.5.dlog$Count
    }
    
    if(!all(is.na(vals))){
      vals <- c(0, vals, f.top(max(vals, na.rm = TRUE)))
      vals.d <- c(0, vals.d, f.top(max(vals.d, na.rm = TRUE)))
    }
    
    ##Save raster data for downloading
    
    if(input$binType == 'Rectangles'){
    
      download.ras.df <- brick(map.layer.pm2.5, map.layer.pm1, map.layer.pm10,
                               map.layer.t, map.layer.h, map.layer.pov, map.layer.tr, map.layer.tc,
                               map.layer.pm2.5.dlog, map.layer.pm1.dlog, map.layer.pm10.dlog,
                               map.layer.t.dlog, map.layer.h.dlog) %>%
        rasterToPoints()
      
      colnames(download.ras.df) <- c('Cell Lng. Center', 'Cell Lat. Center', 'PM2.5', 'PM1', 'PM10', 'Temperature',
                                                              'Humidity', 'ADI', 'AADT', '% Tree cover', 'log10 PM2.5 count',
                                                              'log10 PM1 count', 'log10 PM10 count',
                                                              'log10 Temp. count', 'log10 Hum. count')
 
      
    } else if(input$binType == 'Counties'){
      
      download.ras.df <- full_join(map.layer.pm2.5@data[which(!is.na(map.layer.pm2.5@data[,2])),], 
                                   map.layer.pm1@data[which(!is.na(map.layer.pm1@data[,2])),], 
                                   by = 'NAME') %>% distinct() %>%
        full_join(map.layer.pm10@data[which(!is.na(map.layer.pm10@data[,2])),], by = 'NAME') %>% distinct() %>%
        full_join(map.layer.t@data[which(!is.na(map.layer.t@data[,2])),], by = 'NAME') %>% distinct() %>%
        full_join(map.layer.h@data[which(!is.na(map.layer.h@data[,2])),], by = 'NAME') %>% distinct() %>%
        full_join(map.layer.pov@data[which(!is.na(map.layer.pov@data[,2])),], by = 'NAME') %>% distinct() %>%
        full_join(map.layer.tr@data[which(!is.na(map.layer.tr@data[,2])),], by = 'NAME') %>% distinct() %>%
        full_join(map.layer.tc@data[which(!is.na(map.layer.tc@data[,2])),], by = 'NAME') %>% distinct() %>%
        full_join(map.layer.pm2.5.dlog@data[which(!is.na(map.layer.pm2.5.dlog@data[,2])),], by = 'NAME') %>% distinct() %>%
        full_join(map.layer.pm1.dlog@data[which(!is.na(map.layer.pm1.dlog@data[,2])),], by = 'NAME') %>% distinct() %>%
        full_join(map.layer.pm10.dlog@data[which(!is.na(map.layer.pm10.dlog@data[,2])),], by = 'NAME') %>% distinct() %>%
        full_join(map.layer.t.dlog@data[which(!is.na(map.layer.t.dlog@data[,2])),], by = 'NAME') %>% distinct() %>%
        full_join(map.layer.h.dlog@data[which(!is.na(map.layer.h.dlog@data[,2])),], by = 'NAME')
      
      colnames(download.ras.df) <- c('County', 'PM2.5', 'PM1', 'PM10', 'Temperature',
                                     'Humidity', 'ADI', 'AADT', '% Tree cover', 'log10 PM2.5 count',
                                     'log10 PM1 count', 'log10 PM10 count',
                                     'log10 Temp. count', 'log10 Hum. count')
      
      download.ras.df <- distinct(download.ras.df)
      
    } else if(input$binType == 'ZIP Codes'){
      
      download.ras.df <- full_join(map.layer.pm2.5@data[which(!is.na(map.layer.pm2.5@data[,2])),], 
                                   map.layer.pm1@data[which(!is.na(map.layer.pm1@data[,2])),], 
                                   by = 'ZCTA5CE10') %>% distinct() %>%
        full_join(map.layer.pm10@data[which(!is.na(map.layer.pm10@data[,2])),], by = 'ZCTA5CE10') %>% distinct() %>%
        full_join(map.layer.t@data[which(!is.na(map.layer.t@data[,2])),], by = 'ZCTA5CE10') %>% distinct() %>%
        full_join(map.layer.h@data[which(!is.na(map.layer.h@data[,2])),], by = 'ZCTA5CE10') %>% distinct() %>%
        full_join(map.layer.pov@data[which(!is.na(map.layer.pov@data[,2])),], by = 'ZCTA5CE10') %>% distinct() %>%
        full_join(map.layer.tr@data[which(!is.na(map.layer.tr@data[,2])),], by = 'ZCTA5CE10') %>% distinct() %>%
        full_join(map.layer.tc@data[which(!is.na(map.layer.tc@data[,2])),], by = 'ZCTA5CE10') %>% distinct() %>%
        full_join(map.layer.pm2.5.dlog@data[which(!is.na(map.layer.pm2.5.dlog@data[,2])),], by = 'ZCTA5CE10') %>% distinct() %>%
        full_join(map.layer.pm1.dlog@data[which(!is.na(map.layer.pm1.dlog@data[,2])),], by = 'ZCTA5CE10') %>% distinct() %>%
        full_join(map.layer.pm10.dlog@data[which(!is.na(map.layer.pm10.dlog@data[,2])),], by = 'ZCTA5CE10') %>% distinct() %>%
        full_join(map.layer.t.dlog@data[which(!is.na(map.layer.t.dlog@data[,2])),], by = 'ZCTA5CE10') %>% distinct() %>%
        full_join(map.layer.h.dlog@data[which(!is.na(map.layer.h.dlog@data[,2])),], by = 'ZCTA5CE10')
      
      colnames(download.ras.df) <- c('ZIP Code', 'PM2.5', 'PM1', 'PM10', 'Temperature',
                                     'Humidity', 'ADI', 'AADT', '% Tree cover', 'log10 PM2.5 count',
                                     'log10 PM1 count', 'log10 PM10 count',
                                     'log10 Temp. count', 'log10 Hum. count')
      
      download.ras.df <- distinct(download.ras.df)
      
    } else if(input$binType == 'Block Groups'){
      
      download.ras.df <- full_join(map.layer.pm2.5@data[which(!is.na(map.layer.pm2.5@data[,2])),], 
                                   map.layer.pm1@data[which(!is.na(map.layer.pm1@data[,2])),], 
                                   by = 'AFFGEOID') %>% distinct() %>%
        full_join(map.layer.pm10@data[which(!is.na(map.layer.pm10@data[,2])),], by = 'AFFGEOID') %>% distinct() %>%
        full_join(map.layer.t@data[which(!is.na(map.layer.t@data[,2])),], by = 'AFFGEOID') %>% distinct() %>%
        full_join(map.layer.h@data[which(!is.na(map.layer.h@data[,2])),], by = 'AFFGEOID') %>% distinct() %>%
        full_join(map.layer.pov@data[which(!is.na(map.layer.pov@data[,2])),], by = 'AFFGEOID') %>% distinct() %>%
        full_join(map.layer.tr@data[which(!is.na(map.layer.tr@data[,2])),], by = 'AFFGEOID') %>% distinct() %>%
        full_join(map.layer.tc@data[which(!is.na(map.layer.tc@data[,2])),], by = 'AFFGEOID') %>% distinct() %>%
        full_join(map.layer.pm2.5.dlog@data[which(!is.na(map.layer.pm2.5.dlog@data[,2])),], by = 'AFFGEOID') %>% distinct() %>%
        full_join(map.layer.pm1.dlog@data[which(!is.na(map.layer.pm1.dlog@data[,2])),], by = 'AFFGEOID') %>% distinct() %>%
        full_join(map.layer.pm10.dlog@data[which(!is.na(map.layer.pm10.dlog@data[,2])),], by = 'AFFGEOID') %>% distinct() %>%
        full_join(map.layer.t.dlog@data[which(!is.na(map.layer.t.dlog@data[,2])),], by = 'AFFGEOID') %>% distinct() %>%
        full_join(map.layer.h.dlog@data[which(!is.na(map.layer.h.dlog@data[,2])),], by = 'AFFGEOID')
      
      colnames(download.ras.df) <- c('ZIP Code', 'PM2.5', 'PM1', 'PM10', 'Temperature',
                                     'Humidity', 'ADI', 'AADT', '% Tree cover', 'log10 PM2.5 count',
                                     'log10 PM1 count', 'log10 PM10 count',
                                     'log10 Temp. count', 'log10 Hum. count')
      
      download.ras.df <- distinct(download.ras.df)
      
    }
    
    assign('download.ras.df', as_tibble(download.ras.df), envir = .GlobalEnv)
    
    ## Initialize leaflet -------------
    if(input$binType == 'Rectangles'){
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
        addPolylines(data = display.ctys, color = "black", weight = 1, group = "Counties") %>%
        addPolylines(data = display.zips, color = "black", weight = 1, group = "ZIP Codes") %>%
        addPolylines(data = display.blockGs, color = "black", weight = 1, group = "Block Groups") %>%
        addLayersControl(baseGroups = all.measures.sub, 
                         overlayGroups = c("Measurement value", "Measurement density", "Counties",
                                           "ZIP Codes", "Block Groups"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        showGroup(c("PM\u2082.\u2085", "Measurement value")) %>%
        hideGroup(c(all.measures.sub[which(all.measures.sub != "PM\u2082.\u2085")],
                    "Measurement density", "Counties", "ZIP Codes", "Block Groups"))
  
      #Plot individual maps side-by-side in a grid
      #Make PM2.5 maps
      pm25 <- leaflet(content.df) %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(map.layer.pm2.5, colors = pal.pm2.5, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
        addLegend(pal = leg.pal.pm2.5, values = vals, opacity = 1,
                  title = toString(f.titles("PM2.5")), position = "topright",
                  group = "Measurement value",
                  labFormat = myLabelFormat()) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
      
      #Make PM1 maps
      vals1 <- values(map.layer.pm1)
      pm1 <- leaflet(content.df) %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(map.layer.pm1, colors = pal.pm1, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
        addLegend(pal = leg.pal.pm1, values = vals1, opacity = 1,
                  title = toString(f.titles("PM1")), position = "topright",
                  group = "Measurement value",
                  labFormat = myLabelFormat()) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
      
      #Make PM10 maps
      vals10 <- values(map.layer.pm10)
      pm10 <- leaflet(content.df) %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(map.layer.pm10, colors = pal.pm10, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
        addLegend(pal = leg.pal.pm10, values = vals10, opacity = 1,
                  title = toString(f.titles("PM10")), position = "topright",
                  group = "Measurement value",
                  labFormat = myLabelFormat()) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
      
      #Make temperature maps
      valst <- values(map.layer.t)
      temp <- leaflet(content.df) %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(map.layer.t, colors = pal.t, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
        addLegend(pal = leg.pal.t, values = valst, opacity = 1,
                  title = toString(f.titles("Temperature")), position = "topright",
                  group = "Measurement value",
                  labFormat = myLabelFormat()) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
      
      #Make humidity maps
      valsh <- values(map.layer.h)
      humid <- leaflet(content.df) %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(map.layer.h, colors = pal.h, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
        addLegend(pal = leg.pal.h, values = valsh, opacity = 1,
                  title = toString(f.titles("Humidity")), position = "topright",
                  group = "Measurement value",
                  labFormat = myLabelFormat()) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
      
      #Make crime maps
      valsc <- values(map.layer.c)
      crime <- leaflet(content.df) %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(map.layer.c, colors = pal.c, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
        addLegend(pal = leg.pal.c, values = valsc, opacity = 1,
                  title = toString(f.titles("Crime")), position = "topright",
                  group = "Measurement value",
                  labFormat = myLabelFormat()) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
      
      #Make poverty maps
      valspov <- values(map.layer.pov)
      pov <- leaflet(content.df) %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(map.layer.pov, colors = pal.pov, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
        addLegend(pal = leg.pal.pov, values = valspov, opacity = 1,
                  title = toString(f.titles("Area Deprivation Index")), position = "topright",
                  group = "Measurement value",
                  labFormat = myLabelFormat()) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
      
      #Make traffic maps
      valstr <- values(map.layer.tr)
      tr <- leaflet(content.df) %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(map.layer.tr, colors = pal.tr, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
        addLegend(pal = leg.pal.tr, values = valstr, opacity = 1,
                  title = toString(f.titles("Traffic")), position = "topright",
                  group = "Measurement value",
                  labFormat = myLabelFormat()) %>%      addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
      
      #Make tree cover maps
      valstc <- values(map.layer.tc)
      tc <- leaflet(content.df) %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(map.layer.tc, colors = pal.tc, opacity = 0.8, group = "Measurement value", method = "ngb") %>%
        addLegend(pal = leg.pal.tc, values = valstc, opacity = 1,
                  title = toString(f.titles("Tree Cover")), position = "topright",
                  group = "Measurement value",
                  labFormat = myLabelFormat()) %>%      addEasyButton(easyButton(
                    icon = "fa-crosshairs", title = "Recenter",
                    onClick = JS(paste(button.js))
                  ))
      
      
      ###Make density maps
      valspm25.d <- values(map.layer.pm2.5.dlog)
      pm25.d <- leaflet(content.df) %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(map.layer.pm2.5.dlog, colors = pal.pm2.5.d, opacity = 0.8, method = "ngb") %>%
        addLegend(pal = leg.pal.pm2.5.d, values = valspm25.d, opacity = 1,
                  title = toString(f.titles.d("PM2.5")), position = "topright",
                  labFormat = myLabelFormat()) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
      
      #Make PM1 maps
      valspm1.d <- values(map.layer.pm1.dlog)
      pm1.d <- leaflet(content.df) %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(map.layer.pm1.dlog, colors = pal.pm1.d, opacity = 0.8, method = "ngb") %>%
        addLegend(pal = leg.pal.pm1.d, values = valspm1.d, opacity = 1,
                  title = toString(f.titles.d("PM1")), position = "topright",
                  labFormat = myLabelFormat()) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
      
      #Make PM10 maps
      valspm10.d <- values(map.layer.pm10.dlog)
      pm10.d <- leaflet(content.df) %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(map.layer.pm10.dlog, colors = pal.pm10.d, opacity = 0.8, method = "ngb") %>%
        addLegend(pal = leg.pal.pm10.d, values = valspm10.d, opacity = 1,
                  title = toString(f.titles.d("PM10")), position = "topright",
                  labFormat = myLabelFormat()) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
      
      #Make temperature maps
      valst.d <- values(map.layer.t.dlog)
      temp.d <- leaflet(content.df) %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(map.layer.t.dlog, colors = pal.t.d, opacity = 0.8, method = "ngb") %>%
        addLegend(pal = leg.pal.t.d, values = valst.d, opacity = 1,
                  title = toString(f.titles.d("Temperature")), position = "topright",
                  labFormat = myLabelFormat()) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
      
      #Make humidity maps
      valsh.d <- values(map.layer.h.dlog)
      humid.d <- leaflet(content.df) %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(map.layer.h.dlog, colors = pal.h.d, opacity = 0.8, method = "ngb") %>%
        addLegend(pal = leg.pal.h.d, values = valsh.d, opacity = 1,
                  title = toString(f.titles.d("Humidity")), position = "topright",
                  labFormat = myLabelFormat()) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
      
      #####Make EPA maps
      epa.dates <- input$dates[1]:input$dates[2]
      
      #PM2.5
      epa.pm2.5.ras <- getEPAraster('PM2.5', epa.dates) #Also returns epa.df for downloading data
      epa.pm2.5.ras[epa.pm2.5.ras == -1] <- NA #Non-values are given as -1 by raster function
      epa.pm2.5.ras <- crop(epa.pm2.5.ras, extent(r.shape))
      epa.vals.pm2.5 <- values(epa.pm2.5.ras)
      
      epa.pal.pm2.5 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                              domain = epa.vals.pm2.5,
                              na.color = "transparent"
      )
      epa.leg.pal.pm2.5 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                  domain = epa.vals.pm2.5,
                                  na.color = "transparent",
                                  reverse = TRUE
      )
      
      epa.pm2.5 <- leaflet() %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(epa.pm2.5.ras, colors = epa.pal.pm2.5, opacity = 0.8, method = "ngb") %>%
        addLegend(pal = epa.leg.pal.pm2.5, values = epa.vals.pm2.5, opacity = 1,
                  title = toString(f.titles.epa('PM2.5')), position = "topright",
                  labFormat = myLabelFormat()) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
      
      ##PM10
      epa.pm10.ras <- getEPAraster('PM10', epa.dates) #Also returns epa.df for downloading data
      epa.pm10.ras[epa.pm10.ras == -1] <- NA #Non-values are given as -1 by raster function
      epa.pm10.ras <- crop(epa.pm10.ras, extent(r.shape))
      epa.vals.pm10 <- values(epa.pm10.ras)
      
      epa.pal.pm10 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                   domain = epa.vals.pm10,
                                   na.color = "transparent"
      )
      epa.leg.pal.pm10 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                       domain = epa.vals.pm10,
                                       na.color = "transparent",
                                       reverse = TRUE
      )
      
      epa.pm10 <- leaflet() %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(epa.pm10.ras, colors = epa.pal.pm10, opacity = 0.8, method = "ngb") %>%
        addLegend(pal = epa.leg.pal.pm10, values = epa.vals.pm10, opacity = 1,
                  title = toString(f.titles.epa('PM10')), position = "topright",
                  labFormat = myLabelFormat()) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
      
      ##SO2
      epa.so2.ras <- getEPAraster('SO2', epa.dates) #Also returns epa.df for downloading data
      epa.so2.ras[epa.so2.ras == -1] <- NA #Non-values are given as -1 by raster function
      epa.so2.ras <- crop(epa.so2.ras, extent(r.shape))
      epa.vals.so2 <- values(epa.so2.ras)
      
      epa.pal.so2 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                   domain = epa.vals.so2,
                                   na.color = "transparent"
      )
      epa.leg.pal.so2 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                       domain = epa.vals.so2,
                                       na.color = "transparent",
                                       reverse = TRUE
      )
      
      epa.so2 <- leaflet() %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(epa.so2.ras, colors = epa.pal.so2, opacity = 0.8, method = "ngb") %>%
        addLegend(pal = epa.leg.pal.so2, values = epa.vals.so2, opacity = 1,
                  title = toString(f.titles.epa('SO2')), position = "topright",
                  labFormat = myLabelFormat()) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
      
      ##O3
      epa.o3.ras <- getEPAraster('O3', epa.dates) #Also returns epa.df for downloading data
      epa.o3.ras[epa.o3.ras == -1] <- NA #Non-values are given as -1 by raster function
      epa.o3.ras <- crop(epa.o3.ras, extent(r.shape))
      epa.vals.o3 <- values(epa.o3.ras)
      
      epa.pal.o3 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                   domain = epa.vals.o3,
                                   na.color = "transparent"
      )
      epa.leg.pal.o3 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                       domain = epa.vals.o3,
                                       na.color = "transparent",
                                       reverse = TRUE
      )
      
      epa.o3 <- leaflet() %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(epa.o3.ras, colors = epa.pal.o3, opacity = 0.8, method = "ngb") %>%
        addLegend(pal = epa.leg.pal.o3, values = epa.vals.o3, opacity = 1,
                  title = toString(f.titles.epa('O3')), position = "topright",
                  labFormat = myLabelFormat()) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
      
      ##NO2
      epa.no2.ras <- getEPAraster('NO2', epa.dates) #Also returns epa.df for downloading data
      epa.no2.ras[epa.no2.ras == -1] <- NA #Non-values are given as -1 by raster function
      epa.no2.ras <- crop(epa.no2.ras, extent(r.shape))
      epa.vals.no2 <- values(epa.no2.ras)
      
      epa.pal.no2 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                  domain = epa.vals.no2,
                                  na.color = "transparent"
      )
      epa.leg.pal.no2 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                      domain = epa.vals.no2,
                                      na.color = "transparent",
                                      reverse = TRUE
      )
      
      epa.no2 <- leaflet() %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(epa.no2.ras, colors = epa.pal.no2, opacity = 0.8, method = "ngb") %>%
        addLegend(pal = epa.leg.pal.no2, values = epa.vals.no2, opacity = 1,
                  title = toString(f.titles.epa('NO2')), position = "topright",
                  labFormat = myLabelFormat()) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
      
      ##CO
      epa.co.ras <- getEPAraster('CO', epa.dates) #Also returns epa.df for downloading data
      epa.co.ras[epa.co.ras == -1] <- NA #Non-values are given as -1 by raster function
      epa.co.ras <- crop(epa.co.ras, extent(r.shape))
      epa.vals.co <- values(epa.co.ras)
      
      epa.pal.co <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                  domain = epa.vals.co,
                                  na.color = "transparent"
      )
      epa.leg.pal.co <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                      domain = epa.vals.co,
                                      na.color = "transparent",
                                      reverse = TRUE
      )
      
      epa.co <- leaflet() %>%
        setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
        addProviderTiles(providers$Esri.WorldTopoMap) %>%
        addRasterImage(epa.co.ras, colors = epa.pal.co, opacity = 0.8, method = "ngb") %>%
        addLegend(pal = epa.leg.pal.co, values = epa.vals.co, opacity = 1,
                  title = toString(f.titles.epa('CO')), position = "topright",
                  labFormat = myLabelFormat()) %>%
        addEasyButton(easyButton(
          icon = "fa-crosshairs", title = "Recenter",
          onClick = JS(paste(button.js))
        ))
    
    } else {

        content_map <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addPolygons(data = map.layer.pm2.5, stroke = FALSE, fillOpacity = 0.8, fillColor = ~pal.pm2.5(PM2.5), opacity = 0.8, group = "Measurement value",
                      popup = sp.content) %>%
          addLegend(pal = leg.pal.pm2.5, values = vals, opacity = 1,
                    title = toString(f.titles("PM2.5")), position = "topright",
                    group = "Measurement value",
                    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
          addPolygons(data = map.layer.pm2.5.dlog, stroke = FALSE, fillOpacity = 0.8, fillColor = ~pal.pm2.5.d(Count), opacity = 0.8, group = "Measurement density",
                      popup = sp.content) %>%
          addLegend(pal = leg.pal.pm2.5.d, values = vals.d, opacity = 1, 
                    title = paste("log\u2081\u2080 # of PM<sub>2.5</sub> data points"),
                    group = "Measurement density", position = "topright",
                    labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
          addMeasure(position = "topleft", primaryLengthUnit = "meters", secondaryLengthUnit = "miles",
                     primaryAreaUnit = "sqmeters", secondaryAreaUnit = "sqmiles") %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          )) %>%
          leafem::addMouseCoordinates() %>%
          addPolylines(data = display.ctys, color = "black", weight = 1, group = "Counties") %>%
          addPolylines(data = display.zips, color = "black", weight = 1, group = "ZIP Codes") %>%
          addPolylines(data = display.blockGs, color = "black", weight = 1, group = "Block Groups") %>%
          addLayersControl(baseGroups = all.measures.sub, 
                           overlayGroups = c("Measurement value", "Measurement density", "Counties",
                                             "ZIP Codes", "Block Groups"),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          showGroup(c("PM\u2082.\u2085", "Measurement value")) %>%
          hideGroup(c(all.measures.sub[which(all.measures.sub != "PM\u2082.\u2085")],
                      "Measurement density", "Counties", "ZIP Codes", "Block Groups"))
        
        #Plot individual maps side-by-side in a grid
        #Make PM2.5 maps
        pm25 <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addPolygons(data = map.layer.pm2.5, stroke = FALSE, fillOpacity = 0.8, fillColor = ~pal.pm2.5(PM2.5), opacity = 0.8, group = "Measurement value",
                      popup = sp.content) %>%
          addLegend(pal = leg.pal.pm2.5, values = vals, opacity = 1,
                    title = toString(f.titles("PM2.5")), position = "topright",
                    group = "Measurement value",
                    labFormat = myLabelFormat()) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          ))
        
        #Make PM1 maps
        vals1 <- map.layer.pm1$PM1
        pm1 <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addPolygons(data = map.layer.pm1, stroke = FALSE, fillOpacity = 0.8, fillColor = ~pal.pm1(PM1), opacity = 0.8, group = "Measurement value",
                      popup = sp.content) %>%
          addLegend(pal = leg.pal.pm1, values = vals1, opacity = 1,
                    title = toString(f.titles("PM1")), position = "topright",
                    group = "Measurement value",
                    labFormat = myLabelFormat()) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          ))
        
        #Make PM10 maps
        vals10 <- map.layer.pm10$PM10
        pm10 <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addPolygons(data = map.layer.pm10, stroke = FALSE, fillOpacity = 0.8, fillColor = ~pal.pm10(PM10), opacity = 0.8, group = "Measurement value",
                      popup = sp.content) %>%
          addLegend(pal = leg.pal.pm10, values = vals10, opacity = 1,
                    title = toString(f.titles("PM10")), position = "topright",
                    group = "Measurement value",
                    labFormat = myLabelFormat()) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          ))
        
        #Make temperature maps
        valst <- map.layer.t$Temperature
        temp <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addPolygons(data = map.layer.t, stroke = FALSE, fillOpacity = 0.8, fillColor = ~pal.t(Temperature), opacity = 0.8, group = "Measurement value",
                      popup = sp.content) %>%
          addLegend(pal = leg.pal.t, values = valst, opacity = 1,
                    title = toString(f.titles("Temperature")), position = "topright",
                    group = "Measurement value",
                    labFormat = myLabelFormat()) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          ))
        
        #Make humidity maps
        valsh <- map.layer.h$Humidity
        humid <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addPolygons(data = map.layer.h, stroke = FALSE, fillOpacity = 0.8, fillColor = ~pal.h(Humidity), opacity = 0.8, group = "Measurement value",
                      popup = sp.content) %>%
          addLegend(pal = leg.pal.h, values = valsh, opacity = 1,
                    title = toString(f.titles("Humidity")), position = "topright",
                    group = "Measurement value",
                    labFormat = myLabelFormat()) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          ))
        
        #Make crime maps
        valsc <- map.layer.c$Crime
        crime <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addPolygons(data = map.layer.c, stroke = FALSE, fillOpacity = 0.8, fillColor = ~pal.c(Crime), opacity = 0.8, group = "Measurement value",
                      popup = sp.content) %>%
          addLegend(pal = leg.pal.c, values = valsc, opacity = 1,
                    title = toString(f.titles("Crime")), position = "topright",
                    group = "Measurement value",
                    labFormat = myLabelFormat()) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          ))
        
        #Make poverty maps
        valspov <- map.layer.pov$ADI_NAT
        pov <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addPolygons(data = map.layer.pov, stroke = FALSE, fillOpacity = 0.8, fillColor = ~pal.pov(ADI_NAT), opacity = 0.8, group = "Measurement value",
                      popup = sp.content) %>%
          addLegend(pal = leg.pal.pov, values = valspov, opacity = 1,
                    title = toString(f.titles("Area Deprivation Index")), position = "topright",
                    group = "Measurement value",
                    labFormat = myLabelFormat()) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          ))
        
        #Make traffic maps
        valstr <- map.layer.tr$Traffic
        tr <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addPolygons(data = map.layer.tr, stroke = FALSE, fillOpacity = 0.8, fillColor = ~pal.tr(Traffic), opacity = 0.8, group = "Measurement value",
                      popup = sp.content) %>%
          addLegend(pal = leg.pal.tr, values = valstr, opacity = 1,
                    title = toString(f.titles("Traffic")), position = "topright",
                    group = "Measurement value",
                    labFormat = myLabelFormat()) %>%      addEasyButton(easyButton(
                      icon = "fa-crosshairs", title = "Recenter",
                      onClick = JS(paste(button.js))
                    ))
        
        #Make tree cover maps
        valstc <- map.layer.tc$TreeCover
        tc <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addPolygons(data = map.layer.tc, stroke = FALSE, fillOpacity = 0.8, fillColor = ~pal.tc(TreeCover), opacity = 0.8, group = "Measurement value",
                      popup = sp.content) %>%
          addLegend(pal = leg.pal.tc, values = valstc, opacity = 1,
                    title = toString(f.titles("Tree Cover")), position = "topright",
                    group = "Measurement value",
                    labFormat = myLabelFormat()) %>%      addEasyButton(easyButton(
                      icon = "fa-crosshairs", title = "Recenter",
                      onClick = JS(paste(button.js))
                    ))
        
        ###Make density maps
        valspm25.d <- map.layer.pm2.5.dlog$Count
        pm25.d <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addPolygons(data = map.layer.pm2.5.dlog, stroke = FALSE, fillOpacity = 0.8, fillColor = ~pal.pm2.5.d(Count), opacity = 0.8, group = "Measurement density",
                      popup = sp.content) %>%
          addLegend(pal = leg.pal.pm2.5.d, values = valspm25.d, opacity = 1,
                    title = toString(f.titles.d("PM2.5")), position = "topright",
                    labFormat = myLabelFormat()) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          ))
        
        #Make PM1 maps
        valspm1.d <- map.layer.pm1.dlog$Count
        pm1.d <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addPolygons(data = map.layer.pm1.dlog, stroke = FALSE, fillOpacity = 0.8, fillColor = ~pal.pm1.d(Count), opacity = 0.8, group = "Measurement density",
                      popup = sp.content) %>%
          addLegend(pal = leg.pal.pm1.d, values = valspm1.d, opacity = 1,
                    title = toString(f.titles.d("PM1")), position = "topright",
                    labFormat = myLabelFormat()) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          ))
        
        #Make PM10 maps
        valspm10.d <- map.layer.pm10.dlog$Count
        pm10.d <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addPolygons(data = map.layer.pm10.dlog, stroke = FALSE, fillOpacity = 0.8, fillColor = ~pal.pm10.d(Count), opacity = 0.8, group = "Measurement density",
                      popup = sp.content) %>%
          addLegend(pal = leg.pal.pm10.d, values = valspm10.d, opacity = 1,
                    title = toString(f.titles.d("PM10")), position = "topright",
                    labFormat = myLabelFormat()) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          ))
        
        #Make temperature maps
        valst.d <- map.layer.t.dlog$Count
        temp.d <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addPolygons(data = map.layer.t.dlog, stroke = FALSE, fillOpacity = 0.8, fillColor = ~pal.t.d(Count), opacity = 0.8, group = "Measurement density",
                      popup = sp.content) %>%
          addLegend(pal = leg.pal.t.d, values = valst.d, opacity = 1,
                    title = toString(f.titles.d("Temperature")), position = "topright",
                    labFormat = myLabelFormat()) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          ))
        
        #Make humidity maps
        valsh.d <- map.layer.h.dlog$Count
        humid.d <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addPolygons(data = map.layer.h.dlog, stroke = FALSE, fillOpacity = 0.8, fillColor = ~pal.h.d(Count), opacity = 0.8, group = "Measurement density",
                      popup = sp.content) %>%
          addLegend(pal = leg.pal.h.d, values = valsh.d, opacity = 1,
                    title = toString(f.titles.d("Humidity")), position = "topright",
                    labFormat = myLabelFormat()) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          ))
        
        #####Make EPA maps
        epa.dates <- input$dates[1]:input$dates[2]
        
        #PM2.5
        epa.pm2.5.ras <- getEPAraster('PM2.5', epa.dates) #Also returns epa.df for downloading data
        epa.pm2.5.ras[epa.pm2.5.ras == -1] <- NA #Non-values are given as -1 by raster function
        epa.pm2.5.ras <- crop(epa.pm2.5.ras, extent(r.shape))
        epa.vals.pm2.5 <- values(epa.pm2.5.ras)
        
        epa.pal.pm2.5 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                     domain = epa.vals.pm2.5,
                                     na.color = "transparent"
        )
        epa.leg.pal.pm2.5 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                         domain = epa.vals.pm2.5,
                                         na.color = "transparent",
                                         reverse = TRUE
        )
        
        epa.pm2.5 <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addRasterImage(epa.pm2.5.ras, colors = epa.pal.pm2.5, opacity = 0.8, method = "ngb") %>%
          addLegend(pal = epa.leg.pal.pm2.5, values = epa.vals.pm2.5, opacity = 1,
                    title = toString(f.titles.epa('PM2.5')), position = "topright",
                    labFormat = myLabelFormat()) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          ))
        
        ##PM10
        epa.pm10.ras <- getEPAraster('PM10', epa.dates) #Also returns epa.df for downloading data
        epa.pm10.ras[epa.pm10.ras == -1] <- NA #Non-values are given as -1 by raster function
        epa.pm10.ras <- crop(epa.pm10.ras, extent(r.shape))
        epa.vals.pm10 <- values(epa.pm10.ras)
        
        epa.pal.pm10 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                     domain = epa.vals.pm10,
                                     na.color = "transparent"
        )
        epa.leg.pal.pm10 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                         domain = epa.vals.pm10,
                                         na.color = "transparent",
                                         reverse = TRUE
        )
        
        epa.pm10 <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addRasterImage(epa.pm10.ras, colors = epa.pal.pm10, opacity = 0.8, method = "ngb") %>%
          addLegend(pal = epa.leg.pal.pm10, values = epa.vals.pm10, opacity = 1,
                    title = toString(f.titles.epa('PM10')), position = "topright",
                    labFormat = myLabelFormat()) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          ))
        
        ##SO2
        epa.so2.ras <- getEPAraster('SO2', epa.dates) #Also returns epa.df for downloading data
        epa.so2.ras[epa.so2.ras == -1] <- NA #Non-values are given as -1 by raster function
        epa.so2.ras <- crop(epa.so2.ras, extent(r.shape))
        epa.vals.so2 <- values(epa.so2.ras)
        
        epa.pal.so2 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                    domain = epa.vals.so2,
                                    na.color = "transparent"
        )
        epa.leg.pal.so2 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                        domain = epa.vals.so2,
                                        na.color = "transparent",
                                        reverse = TRUE
        )
        
        epa.so2 <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addRasterImage(epa.so2.ras, colors = epa.pal.so2, opacity = 0.8, method = "ngb") %>%
          addLegend(pal = epa.leg.pal.so2, values = epa.vals.so2, opacity = 1,
                    title = toString(f.titles.epa('SO2')), position = "topright",
                    labFormat = myLabelFormat()) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          ))
        
        ##O3
        epa.o3.ras <- getEPAraster('O3', epa.dates) #Also returns epa.df for downloading data
        epa.o3.ras[epa.o3.ras == -1] <- NA #Non-values are given as -1 by raster function
        epa.o3.ras <- crop(epa.o3.ras, extent(r.shape))
        epa.vals.o3 <- values(epa.o3.ras)
        
        epa.pal.o3 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                   domain = epa.vals.o3,
                                   na.color = "transparent"
        )
        epa.leg.pal.o3 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                       domain = epa.vals.o3,
                                       na.color = "transparent",
                                       reverse = TRUE
        )
        
        epa.o3 <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addRasterImage(epa.o3.ras, colors = epa.pal.o3, opacity = 0.8, method = "ngb") %>%
          addLegend(pal = epa.leg.pal.o3, values = epa.vals.o3, opacity = 1,
                    title = toString(f.titles.epa('O3')), position = "topright",
                    labFormat = myLabelFormat()) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          ))
        
        ##NO2
        epa.no2.ras <- getEPAraster('NO2', epa.dates) #Also returns epa.df for downloading data
        epa.no2.ras[epa.no2.ras == -1] <- NA #Non-values are given as -1 by raster function
        epa.no2.ras <- crop(epa.no2.ras, extent(r.shape))
        epa.vals.no2 <- values(epa.no2.ras)
        
        epa.pal.no2 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                    domain = epa.vals.no2,
                                    na.color = "transparent"
        )
        epa.leg.pal.no2 <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                        domain = epa.vals.no2,
                                        na.color = "transparent",
                                        reverse = TRUE
        )
        
        epa.no2 <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addRasterImage(epa.no2.ras, colors = epa.pal.no2, opacity = 0.8, method = "ngb") %>%
          addLegend(pal = epa.leg.pal.no2, values = epa.vals.no2, opacity = 1,
                    title = toString(f.titles.epa('NO2')), position = "topright",
                    labFormat = myLabelFormat()) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          ))
        
        ##CO
        epa.co.ras <- getEPAraster('CO', epa.dates) #Also returns epa.df for downloading data
        epa.co.ras[epa.co.ras == -1] <- NA #Non-values are given as -1 by raster function
        epa.co.ras <- crop(epa.co.ras, extent(r.shape))
        epa.vals.co <- values(epa.co.ras)
        
        epa.pal.co <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                   domain = epa.vals.co,
                                   na.color = "transparent"
        )
        epa.leg.pal.co <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                       domain = epa.vals.co,
                                       na.color = "transparent",
                                       reverse = TRUE
        )
        
        epa.co <- leaflet() %>%
          setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
          addProviderTiles(providers$Esri.WorldTopoMap) %>%
          addRasterImage(epa.co.ras, colors = epa.pal.co, opacity = 0.8, method = "ngb") %>%
          addLegend(pal = epa.leg.pal.co, values = epa.vals.co, opacity = 1,
                    title = toString(f.titles.epa('CO')), position = "topright",
                    labFormat = myLabelFormat()) %>%
          addEasyButton(easyButton(
            icon = "fa-crosshairs", title = "Recenter",
            onClick = JS(paste(button.js))
          ))
      
    } #End "else"

    #make list of all maps
    maps <- list(main = content_map, pm25=pm25, pm1=pm1, pm10=pm10,temp=temp,
                 humid=humid,crime=crime, pov=pov, tr=tr, tc=tc,
                 pm25.d=pm25.d, pm1.d=pm1.d, pm10.d=pm10.d, temp.d=temp.d, humid.d=humid.d, 
                 epa.pm2.5 = epa.pm2.5,epa.pm10 = epa.pm10, epa.so2 = epa.so2, 
                 epa.no2 = epa.no2,epa.o3 = epa.o3, epa.co = epa.co)
    maps
    
  }) #End eventReactive
  
  #Delays plotting until "Go" button is clicked
  
  observeEvent(input$go, {
    output$int.map <- renderLeaflet({
      withProgress(message = "Loading Main Map...", {
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
      withProgress(message = "Loading Grid Maps...", {
        maps <- map.plot()
        all_maps$dat <- sync(maps$pm25, maps$temp, maps$humid)})
    })
  })
  

  #Make custom map grid
  observeEvent(input$grid.update,{
    maps.list <- paste('maps', input$grid.vars, sep = '$') %>%
      as.list()
    output$all.maps <- renderUI({
      withProgress(message = "Loading Grid Maps...", {
        maps <- map.plot()
        all_maps$dat <- sync(
          lapply(maps.list, function(x) eval(parse(text = x)))
        )})
    })
  })
  
  measure <- reactive({setdiff(input$int.map_groups,
                               c("Measurement value", "Measurement density", 
                                 "Counties", "ZIP Codes", "Block Groups"))})
  
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
      if(input$binType == 'Rectangles'){
        vals <- values(map.layer)
      } else {
        if(length(r) > 0){
          vals <- map.layer[,2]@data %>% unlist() %>% as.double()
        } else {
          vals <- NA
        }
      }
      
      if(measure() %in% all.measures.sub){
        
        if(all(is.na(vals))){
          map %>%
            clearImages() %>%
            clearShapes() %>%
            addLabelOnlyMarkers(
              lng = lon.center, lat = lat.center,
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
          vals <- c(0, vals, f.top(max(vals, na.rm = TRUE)))
          pal <- eval(parse(text = paste0("pal", suffix)))
          leg.pal <- eval(parse(text = paste0("leg.pal", suffix)))
          
          if(input$binType == 'Rectangles'){
            
            map %>%
              clearImages() %>%
              removeMarker("null1") %>%
              addRasterImage(map.layer, colors = pal, opacity = 0.8, method = "ngb") %>%
              addLegend(pal = leg.pal, values = vals, opacity = 1,
                        title = legend.title, position = "topright",
                        labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
              addPolylines(data = display.ctys, color = "black", weight = 1, group = "Counties") %>%
              addPolylines(data = display.zips, color = "black", weight = 1, group = "ZIP Codes") %>%
              addPolylines(data = display.blockGs, color = "black", weight = 1, group = "Block Groups") %>%
              showGroup(c(input$int.map_groups, "Measurement value")) %>%
              hideGroup(c(all.measures.sub[which(all.measures.sub != measure())], "Measurement density"))

          } else {

            map %>%
              clearShapes() %>%
              removeMarker("null1") %>%
              addPolygons(data = map.layer, stroke = FALSE, fillOpacity = 0.8, fillColor = ~pal(eval(parse(text = names(map.layer)[2]))), opacity = 0.8, popup = sp.content) %>%
              addLegend(pal = leg.pal, values = vals, opacity = 1,
                        title = legend.title, position = "topright",
                        labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
              addPolylines(data = display.ctys, color = "black", weight = 1, group = "Counties") %>%
              addPolylines(data = display.zips, color = "black", weight = 1, group = "ZIP Codes") %>%
              addPolylines(data = display.blockGs, color = "black", weight = 1, group = "Block Groups") %>%
              showGroup(c(input$int.map_groups, "Measurement value")) %>%
              hideGroup(c(all.measures.sub[which(all.measures.sub != measure())], "Measurement density"))
            
            
            
          }
        }
      }
    }
    
    ## Measurement density selected -------------
    else if("Measurement density" %in% input$int.map_groups && !"Measurement value" %in% input$int.map_groups){
      
      if(measure() %in% sensor.measures.sub){
        
        meas.text <- f.plaintext(measure())
        suffix <- f.suffix(meas.text)
        map.layer <- eval(parse(text = paste0("map.layer", suffix, ".dlog")))
        legend.title <- toString(f.titles.d(measure()))
        
        if(input$binType == 'Rectangles'){
          vals <- values(map.layer)
        } else {
          vals <- map.layer[,2]@data %>% unlist() %>% as.double()
        }
        
        if(all(is.na(vals))){
          map %>%
            clearImages() %>%
            clearShapes() %>%
            addLabelOnlyMarkers(
              lng = lon.center, lat = lat.center,
              label = "No data",
              layerId = "null1",
              labelOptions = labelOptions(noHide = TRUE,
                                          style = list(
                                            "color" = "red",
                                            "font-size" = "20px",
                                            "font-family" = "serif",
                                            "border-color" = "rgba(0,0,0,1)"
                                          )))
        } else{
          
          pal <- eval(parse(text = paste0("pal", suffix, ".d")))
          leg.pal <- eval(parse(text = paste0("leg.pal", suffix, ".d")))
          
          if(input$binType == 'Rectangles'){
            vals <- values(map.layer)
          } else {
            vals <- map.layer[,2]@data %>% unlist() %>% as.double()
          }
          vals <- c(0, vals, f.top(max(vals, na.rm = TRUE)))
          
          if(input$binType == 'Rectangles'){
            
            map %>%
              clearImages() %>%
              removeMarker("null1") %>%
              addRasterImage(map.layer, colors = pal, opacity = 0.8, method = "ngb") %>%
              addLegend(pal = leg.pal, values = vals, opacity = 1,
                        title = legend.title, position = "topright",
                        labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
              addPolylines(data = display.ctys, color = "black", weight = 1, group = "Counties") %>%
              addPolylines(data = display.zips, color = "black", weight = 1, group = "ZIP Codes") %>%
              addPolylines(data = display.blockGs, color = "black", weight = 1, group = "Block Groups") %>%
              showGroup(c(measure(), "Measurement density")) %>%
              hideGroup(c(all.measures.sub[which(all.measures.sub != measure())], 
                          "Measurement value"))
            
          } else {
            
            map %>%
              clearShapes() %>%
              removeMarker("null1") %>%
              addPolygons(data = map.layer, stroke = FALSE, fillOpacity = 0.8, fillColor = ~pal(Count), opacity = 0.8, popup = sp.content) %>%
              addLegend(pal = leg.pal, values = vals, opacity = 1,
                        title = legend.title, position = "topright",
                        labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
              addPolylines(data = display.ctys, color = "black", weight = 1, group = "Counties") %>%
              addPolylines(data = display.zips, color = "black", weight = 1, group = "ZIP Codes") %>%
              addPolylines(data = display.blockGs, color = "black", weight = 1, group = "Block Groups") %>%
              showGroup(c(measure(), "Measurement density")) %>%
              hideGroup(c(all.measures.sub[which(all.measures.sub != measure())], 
                          "Measurement value"))
          } 
        }
        
        # Check for crime, poverty and traffic -------------
      } else if(measure() %in% other.measures){ 
        map %>%
          clearImages() %>%
          clearShapes() %>%
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
        clearShapes() %>%
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
        clearShapes() %>%
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
  
  epa.plot <- eventReactive(input$go, {
    
    epa.dates <- input$dates[1]:input$dates[2]
    
    epa.download.df <- EPA_data %>%
      dplyr::filter(Date %in% epa.dates) %>%
      dplyr::distinct()
    
    assign('epa.download.df',epa.download.df, envir = .GlobalEnv)
    
    for(i in 1:nrow(epa.titles.df)){
      
      cur.var <- epa.titles.df[i,1]
      
      epa.ras <- getEPAraster(cur.var, epa.dates) %>%
        crop(extent(r.shape)) %>% mask(r.shape)
      #Also returns epa.df for downloading data
      epa.ras[epa.ras == -1] <- NA #Non-values are given as -1 by raster function
      
      mon.frame <- epa.df %>%
        dplyr::select(Longitude, Latitude, AQS_Site_ID, Local.Site.Name,
               State.Name, City.Name, Monitor_Start_Date, Last_Sample_Date) %>%
        dplyr::distinct()
      #Gives just monitor stats (removes repetition from dates, variables and measurements)

      #Give correct values for start and end of EPA sampling range
      mon.frame$Monitor_Start_Date[which(
        mon.frame$Monitor_Start_Date < input$EPA_dates[1])] <- input$EPA_dates[1]
      mon.frame$Last_Sample_Date[which(
        mon.frame$Last_Sample_Date > input$EPA_dates[2])] <- input$EPA_dates[2]
      
      assign(paste0('mon.frame.', cur.var), mon.frame, envir = .GlobalEnv)
      
      vals <- values(epa.ras)
      if(all(is.na(vals))){vals <- c(0, vals)}
      
      assign(paste0('epa.ras.', cur.var), epa.ras, envir = .GlobalEnv)
    
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
      epa.val <- paste0('<b>Interpolated ', f.titles.epa(cur.var),
                        ': <b style = \"color:DodgerBlue\">', 
                        round(values(epa.ras), digits = 2),"</b></b>")
      
      ## Final content vector -------------
      epa.content <- paste0(site, id, city_state, lat_lon, avg.range)
      
      #Removes popups for which all data are NA
      #Coercing lat and lon to NA works better than removing these rows
      epa.content.df <- data.frame(cbind(lons, lats, 
                                         epa.content), stringsAsFactors = FALSE)
      epa.content.df[,1:2] <- sapply(epa.content.df[,1:2], as.numeric)
      assign(paste0('epa.content.df.', cur.var), epa.content.df, envir = .GlobalEnv)
      #-----------
      
      #df of interpolated values to display
      epa.val.df <- data.frame(cbind(epa.lons, epa.lats, epa.val), 
                               stringsAsFactors = FALSE)
      epa.val.df[,1:2] <- sapply(epa.val.df[,1:2], as.numeric)
      epa.val.df[which(is.na(values(epa.ras))), 1:2] <- NA
      assign(paste0('epa.val.df.', cur.var), epa.val.df, envir = .GlobalEnv)
      #Coerces lats and lons to NA where vals are NA
      
      pal.epa <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                              domain = vals,
                              na.color = "transparent"
                          )
      
      assign(paste0('pal.epa.', cur.var), pal.epa, envir = .GlobalEnv)
      
      leg.pal.epa <- colorNumeric(palette = brewer.pal(7, "YlOrRd"),
                                  domain = vals,
                                  na.color = "transparent",
                                  reverse = TRUE
                          )
      
      assign(paste0('leg.pal.epa.', cur.var), pal.epa, envir = .GlobalEnv)
      
    }
    
    epa.mon.pal <- colorFactor(palette = "blue", domain = "EPA Monitor Locations")
    
    
    lon.center <- -75.15
    lat.center <- 40.00
    zoom.no <- 8
    button.js <- paste0("function(btn, map){ map.setView([", 
                        lat.center, ", ", lon.center, "], ", zoom.no, "); }")
    
                            
    leaflet(epa.content.df.PM2.5) %>%
      setView(lng = lon.center, lat = lat.center, zoom = zoom.no) %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addRasterImage(epa.ras.PM2.5, colors = pal.epa.PM2.5, opacity = 0.8, method = "ngb") %>%
      addLegend(pal = leg.pal.epa.PM2.5, values = vals, opacity = 1,
                title = f.titles.epa('PM2.5'), position = "topright",
                labFormat = myLabelFormat()) %>%
      addCircleMarkers(data = epa.val.df.PM2.5, lng = ~epa.lons, lat = ~epa.lats, popup = ~epa.val,
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
      addPolylines(data = display.ctys, color = "black", weight = 1, group = "Counties") %>%
      addPolylines(data = display.zips, color = "black", weight = 1, group = "ZIP Codes") %>%
      addPolylines(data = display.blockGs, color = "black", weight = 1, group = "Block Groups") %>%
      addLayersControl(baseGroups = epa.titles.df[,3],
                       overlayGroups = c("Monitor Locations", "Counties", "ZIP Codes", "Block Groups"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      showGroup(c('PM\u2082.\u2085', 'Monitor Locations')) %>%
      hideGroup(c('Counties', 'ZIP Codes', 'Block Groups',
                  epa.titles.df[which(epa.titles.df[,3] != epa.measure()),3]))
  })  
  
  observeEvent(input$go, {
    output$int.map.epa <- renderLeaflet({
      withProgress(message = "Loading EPA Map...", {epa.plot()})
    })
  })
  
  epa.measure <- reactive({setdiff(input$int.map.epa_groups, 
                                   c('Monitor Locations', 'Counties',
                                     'ZIP Codes', 'Block Groups'))})
  
  observeEvent(input$int.map.epa_groups, {
    
    epa.map <- leafletProxy("int.map.epa", session) %>%
      clearControls() %>%
      clearPopups()
    
    epa.meas.text <- f.plaintext(epa.measure())
    epa.ras <- eval(parse(text = paste0('epa.ras.', epa.meas.text)))
    epa.leg.title <- f.titles.epa(epa.meas.text)
    pal.epa <- eval(parse(text = paste0('pal.epa.', epa.meas.text)))
    leg.pal.epa <- eval(parse(text = paste0('leg.pal.epa.', epa.meas.text)))
    vals <- values(epa.ras)
    epa.val.df <- eval(parse(text = paste0('epa.val.df.', epa.meas.text)))
    epa.content.df <- eval(parse(text = paste0('epa.content.df.', epa.meas.text)))
    
    
    epa.map %>%
      clearImages() %>%
      clearShapes() %>%
      clearMarkers() %>%
      addRasterImage(epa.ras, colors = pal.epa, opacity = 0.8, method = "ngb") %>%
      addLegend(pal = leg.pal.epa, values = vals, opacity = 1,
                title = epa.leg.title, position = "topright",
                labFormat = myLabelFormat()) %>%
      addCircleMarkers(data = epa.val.df, lng = ~epa.lons, lat = ~epa.lats, popup = ~epa.val,
                       fillOpacity = 0.0001, stroke = FALSE) %>% #Add these first so they're lowest down
      addCircleMarkers(data = epa.content.df, lng = ~lons, lat = ~lats,
                       radius = 3, color = "blue", fillOpacity = 0.5, stroke = FALSE,
                       group = "Monitor Locations") %>%
      ###Add in transparent circle markers that are larger for easier clicking
      addCircleMarkers(data = epa.content.df, lng = ~lons, lat = ~lats, popup = ~epa.content,
                       radius = 5, color = "transparent", fillOpacity = 0.0001, stroke = FALSE,
                       group = "Monitor Locations") %>% 
      addMeasure(position = "topleft", primaryLengthUnit = "meters", secondaryLengthUnit = "miles",
                 primaryAreaUnit = "sqmeters", secondaryAreaUnit = "sqmiles") %>%
      addPolylines(data = display.ctys, color = "black", weight = 1, group = "Counties") %>%
      addPolylines(data = display.zips, color = "black", weight = 1, group = "ZIP Codes") %>%
      addPolylines(data = display.blockGs, color = "black", weight = 1, group = "Block Groups") %>%
      showGroup(c(epa.measure(), 'Monitor Locations')) %>%
      hideGroup(c(epa.titles.df[which(epa.titles.df[,3] != epa.measure()),3]))
    
  })
  
  ###Data downloader
  output$download <- downloadHandler(
    filename = function() {
      paste0(strftime(Sys.time(), format = '%Y%m%d%H%M%S'), "-Data.csv")
    },
    content = function(file) {
      withProgress(message = 'Preparing download...', 
        write.csv(
          dplyr::filter(download.data, point.in.SpatialPolygons(Longitude, Latitude, r.shape)), 
          file, row.names = FALSE)
      )
    }
  )
  
  output$download.ras <- downloadHandler(
    filename = function() {
      paste0(strftime(Sys.time(), format = '%Y%m%d%H%M%S'), "-RasterData.csv")
    },
    content = function(file) {
      withProgress(message = 'Preparing download...', 
                   write.csv(download.ras.df, file, row.names = FALSE)
      )
    }
  )
  
  
  output$EPA_download <- downloadHandler(
    filename = function() {
      startdate <- strftime(min(epa.download.df$Date), format = '%Y%m%d')
      enddate <- strftime(max(epa.download.df$Date), format = '%Y%m%d')
      paste0('EPA', startdate, "-", enddate, ".csv")
    },
    content = function(file) {
      withProgress(message = 'Preparing download...', 
                   write.csv(epa.download.df, file, row.names = FALSE)
      )
    }
  )
  
}#End server function