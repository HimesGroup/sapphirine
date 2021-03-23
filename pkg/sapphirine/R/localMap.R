#' Local Raster Leaflet Map
#'
#' Renders a `leaflet` map display for brick of local data rasters.
#'
#' @param rasBrick An output of `sapphirine::localRaster`
#' @param popups Whether to include clickable popup displays with summary data for every raster bin in the map display.
#' @param bounds An optional shapefile input to project onto the map display.
#' @param colors Color palette for raster layers
#' @param d.colors Color palette for measurement density layers. Relevant only if `sapphirine::localRaster` output contains density plots.
#' @import leaflet
#' @import RColorBrewer
#' @import sp
#' @import lubridate
#' @export
#' @examples
#'
#' #Select counties for shape
#' counties <- c('Bucks', 'Chester', 'Delaware', 'Montgomery', 'Philadelphia')
#' cty.shape <- selectGPACounties(counties)
#'
#' #Customize subset of local.data
#' dat <- customLocalData(cty.shape, '2017-06-01', '2019-05-31')
#'
#' #Make raster brick
#' localRaster(dat, cty.shape, 100, 100)
#'
#' #Select Philadelphia shapefile to display Philadelphia boundaries in map
#' phl.bound <- selectGPACounties('Philadelphia')
#'
#' localMap(ras, bounds = phl.bound)

localMap <- function(rasBrick, popups = TRUE, bounds = NULL,
                     colors = 'YlOrRd', d.colors = 'Purples'){

  names(rasBrick) <- gsub('log.density.', 'd', names(rasBrick))

  titles.df <- data.frame(
    variables = c('Temperature', 'Humidity', 'PM1', 'PM2.5', 'PM10', 'Crime',
                  'Poverty', 'Traffic'),
    leg.titles = c("Avg. Temp. (\u00B0C)", "Avg. Humidity (%)",
                   "Avg. PM\u2081 Conc. (\u03BCg/m\u00B3)",
                   "Avg. PM\u2082.\u2085 Conc. (\u03BCg/m\u00B3)",
                   "Avg. PM\u2081\u2080 Conc. (\u03BCg/m\u00B3)",
                   "# of Reported Crimes", "Avg. ADI", "Avg. AADT"),
    d.names = c('Temp. density', 'Hum. density', 'PM1 density',
                 'PM2.5 density', 'PM10 density', '', '', ''),
    html.titles = c('Avg. temperature', 'Avg. humidity', 'Avg. PM<sub>1</sub> conc.',
                    'Avg. PM<sub>2.5</sub> conc.', 'Avg. PM<sub>10</sub> conc.',
                    '# of reported crimes', 'Avg. ADI', 'Avg. AADT'),
    units = c('\u00B0C', '%', '\u03BCg/m\u00B3', '\u03BCg/m\u00B3',
              '\u03BCg/m\u00B3', '', '', '')
  )

  non.pols <- c('Crime', 'Poverty', 'Traffic')

  layer.inds <- which(titles.df$variables %in% names(rasBrick))
  layer.names <- c(titles.df$variables[layer.inds], titles.df$d.names[layer.inds])
  layer.names <- layer.names[!layer.names %like% '']

  subsc.layers <- layer.names %>%
    gsub(pattern = 'PM10', replacement = 'PM\u2081\u2080') %>%
    gsub(pattern = 'PM1', replacement = 'PM\u2081') %>%
    gsub(pattern = 'PM2.5', replacement = 'PM\u2082.\u2085')

  colors <- brewer.pal(7, colors)
  colors.d <- brewer.pal(7, d.colors)

  lon.center <- (xmin(rasBrick) + xmax(rasBrick)) / 2
  lat.center <- (ymin(rasBrick) + ymax(rasBrick)) / 2
  zoom.no <- f.zoom(xmax(rasBrick) - xmin(rasBrick), ymax(rasBrick) - ymin(rasBrick))
  button.js <- paste0("function(btn, map){ map.setView([",
                      lat.center, ", ", lon.center, "], ", zoom.no, "); }")

  if(popups){

    lons <- xFromCell(rasBrick[[1]], 1:ncell(rasBrick))
    lats <- yFromCell(rasBrick[[1]], 1:ncell(rasBrick))

    step.size.x <- (xmax(rasBrick) - xmin(rasBrick)) / ncol(rasBrick)
    step.size.y <- (ymax(rasBrick) - ymin(rasBrick)) / nrow(rasBrick)

    total_length <- seq(1, length(values(rasBrick[[1]])))

    lat_lon <- vector()
    lat_lon <- paste0("<b>",
                      "Lat rng: [", "<b style = \"color:DimGray\">",gsub(" ", "", format(round(lats[total_length] - step.size.y/2, 5), nsmall = 5), fixed = TRUE), "</b>", ", ",
                      "<b style = \"color:DimGray\">",gsub(" ", "", format(round(lats[total_length] + step.size.y/2, 5), nsmall = 5), fixed = TRUE), "</b>", "]", "<br/>",
                      "Lon rng: [", "<b style = \"color:DimGray\">",gsub(" ", "", format(round(lons[total_length] - step.size.x/2, 5), nsmall = 5), fixed = TRUE), "</b>", ", ",
                      "<b style = \"color:DimGray\">",gsub(" ", "", format(round(lons[total_length] + step.size.x/2, 5), nsmall = 5), fixed = TRUE), "</b>", "]", "<br/>")

    content <- lat_lon
    rm(lat_lon)

    na.inds <- apply(rasBrick@data@values, 1, function(x) all(is.na(x)))

  }

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
    addLayersControl(overlayGroups = subsc.layers,
                     options = layersControlOptions(collapsed = FALSE))

  for(i in 1:nlayers(rasBrick)){

    cur.layer <- rasBrick[[i]]

    name <- names(rasBrick)[i]

    vals <- values(cur.layer)

    if(!grepl('\\.d', name)){
      final.colors <- colors
      leg.title <- titles.df[which(titles.df$variables == name), 2]
    } else {
      final.colors <- colors.d
      leg.title <- paste('log\u2081\u2080 # of',
                         gsub('\\.d', '', name), 'points')
    }

    if(!all(is.na(vals))){

      if(name != 'Poverty'){

        vals <- c(0, vals, f.top(max(vals, na.rm = TRUE)))

      } else {
         vals <- c(0, vals, 100)
      }


      assign('pal',
             colorNumeric(palette = final.colors,
                          domain = vals,
                          na.color = "transparent"))
      assign('leg.pal',
             colorNumeric(palette = final.colors,
                          domain = vals,
                          na.color = "transparent",
                          reverse = TRUE))
    } else{
      assign('pal',
             colorNumeric(palette = final.colors,
                          domain = 0,
                          na.color = "transparent"))
      assign('leg.pal',
             colorNumeric(palette = final.colors,
                          domain = 0,
                          na.color = "transparent",
                          reverse = TRUE))
    }

    if(!grepl('\\.d', name)){
      name.row <- which(titles.df$variables == name)
      grp.name <- subsc.layers[name.row]
    } else {
      name.row <- which(titles.df$variables == gsub('\\.d', '', name))
      grp.name <- subsc.layers[name.row + length(layer.inds)]
    }

    map <- map %>%
      addRasterImage(cur.layer, colors = pal,
                     opacity = 0.8, group = grp.name, method = "ngb") %>%
      addLegend(pal = leg.pal, values = vals, opacity = 1,
                title = leg.title, position = "topright",
                group = grp.name,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE)))

    if(popups & !grepl('\\.d', name)){

      name.row <- which(titles.df$variables == name)

      content.layer <- vector()
      html.name <- titles.df$html.titles[name.row]
      unit <- titles.df$units[name.row]

      content.layer[which(!is.na(values(cur.layer)))] <- paste0(html.name, ': ', '<b style = \"color:DodgerBlue\">', round(values(cur.layer)[which(!is.na(values(cur.layer)))], digits = 1), unit, '</b>')

      content.layer[which(is.na(values(cur.layer)))] <- paste0(html.name, ': ', '<b style = \"color:Tomato\">', 'no data', "</b>", "<br/>")

      if(titles.df$d.names[name.row] %in% layer.names){
        d.layer <- grep(paste0(name, '.d'), names(rasBrick))
        d.vals <- values(rasBrick[[d.layer]])
        content.layer[which(!is.na(values(cur.layer)))] <-
          paste0(content.layer[which(!is.na(values(cur.layer)))],
                 ' (', round(10**(d.vals[which(!is.na(d.vals))]), digits = 0), ')','<br/>')
      } else {
        content.layer[which(!is.na(values(cur.layer)))] <-
          paste0(content.layer[which(!is.na(values(cur.layer)))], '<br/>')
      }

      if(i == nlayers(rasBrick)){
        content.layer <- paste0(content.layer, '</b>')
      }

      assign('content', paste0(content, content.layer))

      rm(content.layer)

    }

  }

  if(popups){

    content.df <- data.frame(cbind(lons, lats, content), stringsAsFactors = FALSE)
    content.df[,1:2] <- sapply(content.df[,1:2], as.numeric)
    content.df[na.inds, 1:2] <- NA

    map <- addCircleMarkers(map, data = content.df,
                            ~lons, ~lats, popup = ~content,
                            stroke = FALSE, fillOpacity = 0.0001)

  }

  if(!is.null(bounds)){

    if(!grepl('SpatialPolygons', class(bounds))){
      stop('bounds must be of class \"SpatialPolygons\" or \"SpatialPolygonsDataFrame\"')
    } else{

      map <- addPolylines(map, data = bounds, color = 'black', weight = 2)

    }

  }

  map <- hideGroup(map, subsc.layers[-1])

  return(map)

}
