localRaster <- function(data, shape, nrows, ncols,
                        variables = c('Temperature', 'Humidity', 
                                      'PM1', 'PM2.5','PM10', 
                                      'Crime','Poverty', 'Traffic'), 
                         includeCount = TRUE){

  if(class(shape) == 'Extent'){
    shape <- as(shape, 'SpatialPolygons')
    crs(shape) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  } else if(grepl('SpatialPolygons', class(shape))){
    crs(shape) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  } else if(!grepl('SpatialPolygons', class(shape))){
    stop('shape must be of class \"Extent,\" \"SpatialPolygons,\" or \"SpatialPolygonsDataFrame\"')
  }
  
  r <- raster(nrows = nrows, ncols = ncols, xmn = xmin(shape), 
              xmx = xmax(shape), ymn = ymin(shape), ymx = ymax(shape))
  crs(r) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  
  df.variables <- variables[!variables %in% c('Poverty', 'Traffic')]
  
  for(i in 1:length(df.variables)){
    measure.data <- dplyr::filter(data, !is.na(eval(parse(text = df.variables[i]))))
    if(nrow(measure.data) > 0){
      if(includeCount & df.variables[i] != 'Crime'){
        assign("density.raster", 
               rasterize(measure.data[,3:2], r, measure.data$Count, fun = sum, na.rm = TRUE))
      }
      assign(paste(df.variables[i], 'layer', sep = '.'),
             rasterize(measure.data[,3:2], r, measure.data[,df.variables[i]], fun = mean, na.rm = TRUE))
    } else{
      if(includeCount & df.variables[i] != 'Crime'){
        assign("density.raster",
               rasterize(data.frame(NA, NA), r, na.rm = TRUE))
      }
      assign(paste(df.variables[i], 'layer', sep = '.'),
             rasterize(data.frame(NA, NA), r, na.rm = TRUE))
    }       
    if(includeCount & df.variables[i] != 'Crime'){
      assign(paste(df.variables[i], 'density', sep = '.'),
             calc(density.raster, fun = function(x){log10(x)}))
    }
    
    if(df.variables[i] == 'Crime'){
      assign('Crime.layer',
             rasterize(measure.data[,3:2], r,  measure.data[,'Crime'],
                       fun = sum, na.rm = TRUE))
    }
    
    cur.ras <- eval(parse(text = paste(df.variables[i], 'layer', sep = '.'))) %>%
      crop(extent(shape)) %>%
      mask(shape)
    
    if(i == 1){
      ras.brick <- brick(cur.ras)
    } else{
      ras.brick[[i]] <- cur.ras
    }
  }
  
  names(ras.brick) <- df.variables
  
  if(includeCount){
    
    for(j in 1:length(df.variables)){
      
      if(df.variables[j] == 'Crime'){next}
      
      cur.ras <- eval(parse(text = paste(df.variables[j], 'density', sep = '.'))) %>%
        crop(extent(shape)) %>%
        mask(shape)
      
      ras.brick[[1 + nlayers(ras.brick)]] <- cur.ras
      
    }
    
    brick.size <- ifelse('Crime' %in% df.variables, 2*length(df.variables) - 1, 
                         2*length(df.variables))
    
    names(ras.brick)[(length(df.variables)+1):brick.size] <- 
      paste(setdiff(df.variables, 'Crime'), 'log(density)', sep = '.')
    
  }
  
  if('Poverty' %in% variables){
    assign("poverty.layer", 
           try(resample(sapphirine::poverty.raster, r, method = "bilinear"), silent = TRUE))
    if(length(poverty.layer) == 1){
      assign("poverty.layer", rasterize(data.frame(NA, NA), r, na.rm = TRUE))
    }
    ras.brick[[1 + nlayers(ras.brick)]] <- poverty.layer
    names(ras.brick)[[nlayers(ras.brick)]] <- 'Poverty'
  }
  
  if('Traffic' %in% variables){
    assign("traffic.layer", 
           try(resample(sapphirine::traffic.raster, r, method = "bilinear"), silent = TRUE))
    if(length(traffic.layer) == 1){
      assign("traffic.layer", rasterize(data.frame(NA, NA), r, na.rm = TRUE))
    }
    ras.brick[[1 + nlayers(ras.brick)]] <- traffic.layer
    names(ras.brick)[[nlayers(ras.brick)]] <- 'Traffic'
  }
  
  return(ras.brick)
  
}