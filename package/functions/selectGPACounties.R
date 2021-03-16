selectGPACounties <- function(counties){
  
  for(i in 1:length(counties)){
    
    if(!counties[i] %in% GPACountyNames){
      stop('counties must be a vector of county names included in GPACountyNames')
    }
    
    shp.layer <- GPA_counties[GPA_counties$NAME == counties[i],]
    
    if(i == 1){
      newShape <- shp.layer
    } else {
      newShape <- rbind(newShape, shp.layer)
    }
    
  }
  
  return(newShape)
  
}